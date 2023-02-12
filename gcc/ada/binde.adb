------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                B I N D E                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2023, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Binderr; use Binderr;
with Butil;   use Butil;
with Debug;   use Debug;
with Fname;   use Fname;
with Opt;     use Opt;
with Osint;
with Output;  use Output;
with Table;
with Types;   use Types;

with System.Case_Util; use System.Case_Util;
with System.HTable;

package body Binde is
   use Unit_Id_Tables;

   --  We now have Elab_New, a new elaboration-order algorithm.
   --
   --  However, any change to elaboration order can break some programs.
   --  Therefore, we are keeping the old algorithm in place, to be selected
   --  by switches.
   --
   --  The new algorithm has the following interesting properties:
   --
   --    * The static and dynamic models use the same elaboration order. The
   --      static model might get an error, but if it does not, it will use
   --      the same order as the dynamic model.
   --
   --    * Each SCC (see below) is elaborated together; that is, units from
   --      different SCCs are not interspersed.
   --
   --    * In particular, this implies that if an SCC contains just a spec and
   --      the corresponding body, and nothing else, the body will be
   --      elaborated immediately after the spec. This is expected to result
   --      in a better elaboration order for most programs, because in this
   --      case, a call from outside the library unit cannot get ABE.
   --
   --    * Pragmas Elaborate_All (explicit and implicit) are ignored. Instead,
   --      we behave as if every legal pragma Elaborate_All were present. That
   --      is, if it would be legal to have "pragma Elaborate_All(Y);" on X,
   --      then we behave as if such a pragma exists, even if it does not.

   Do_Old : constant Boolean := False;
   Do_New : constant Boolean := True;
   --  True to enable the old and new algorithms, respectively. Used for
   --  debugging/experimentation.

   Doing_New : Boolean := False;
   --  True if we are currently doing the new algorithm. Print certain
   --  messages only when doing the "new" elab order algorithm, so we don't get
   --  duplicates. And use different heuristics in Better_Choice_Optimistic.

   --  The following data structures are used to represent the graph that is
   --  used to determine the elaboration order (using a topological sort).

   --  The following structures are used to record successors. If B is a
   --  successor of A in this table, it means that A must be elaborated before
   --  B is elaborated. For example, if Y (body) says "with X;", then Y (body)
   --  will be a successor of X (spec), and X (spec) will be a predecessor of
   --  Y (body).
   --
   --  Note that we store the successors of each unit explicitly. We don't
   --  store the predecessors, but we store a count of them.
   --
   --  The basic algorithm is to first compute a directed graph of units (type
   --  Unit_Node_Record, below), with successors as edges. A unit is "ready"
   --  (to be chosen as the next to be elaborated) if it has no predecessors
   --  that have not yet been chosen. We use heuristics to decide which of the
   --  ready units should be elaborated next, and "choose" that one (which
   --  means we append it to the elaboration-order table).

   type Successor_Id is new Nat;
   --  Identification of single successor entry

   No_Successor : constant Successor_Id := 0;
   --  Used to indicate end of list of successors

   type Elab_All_Id is new Nat;
   --  Identification of Elab_All entry link

   No_Elab_All_Link : constant Elab_All_Id := 0;
   --  Used to indicate end of list

   --  Succ_Reason indicates the reason for a particular elaboration link

   type Succ_Reason is
     (Withed,
      --  After directly with's Before, so the spec of Before must be
      --  elaborated before After is elaborated.

      Forced,
      --  Before and After come from a pair of lines in the forced-elaboration-
      --  order file.

      Elab,
      --  After directly mentions Before in a pragma Elaborate, so the body of
      --  Before must be elaborated before After is elaborated.

      Elab_All,
      --  After either mentions Before directly in a pragma Elaborate_All, or
      --  mentions a third unit, X, which itself requires that Before be
      --  elaborated before unit X is elaborated. The Elab_All_Link list traces
      --  the dependencies in the latter case.

      Elab_All_Desirable,
      --  This is just like Elab_All, except that the Elaborate_All was not
      --  explicitly present in the source, but rather was created by the front
      --  end, which decided that it was "desirable".

      Elab_Desirable,
      --  This is just like Elab, except that the Elaborate was not explicitly
      --  present in the source, but rather was created by the front end, which
      --  decided that it was "desirable".

      Spec_First);
      --  After is a body, and Before is the corresponding spec

   --  Successor_Link contains the information for one link

   type Successor_Link is record
      Before : Unit_Id;
      --  Predecessor unit

      After : Unit_Id;
      --  Successor unit

      Next : Successor_Id;
      --  Next successor on this list

      Reason : Succ_Reason;
      --  Reason for this link

      Elab_Body : Boolean;
      --  Set True if this link is needed for the special Elaborate_Body
      --  processing described below.

      Reason_Unit : Unit_Id;
      --  For Reason = Elab, or Elab_All or Elab_Desirable, records the unit
      --  containing the pragma leading to the link.

      Elab_All_Link : Elab_All_Id;
      --  If Reason = Elab_All or Elab_Desirable, then this points to the
      --  first element in a list of Elab_All entries that record the with
      --  chain resulting in this particular dependency.
   end record;

   --  Note on handling of Elaborate_Body. Basically, if we have a pragma
   --  Elaborate_Body in a unit, it means that the spec and body have to be
   --  handled as a single entity from the point of view of determining an
   --  elaboration order. What we do is to essentially remove the body from
   --  consideration completely, and transfer all its links (other than the
   --  spec link) to the spec. Then when the spec gets chosen, we choose the
   --  body right afterwards. We mark the links that get moved from the body to
   --  the spec by setting their Elab_Body flag True, so that we can understand
   --  what is going on.

   Succ_First : constant := 1;

   package Succ is new Table.Table
     (Table_Component_Type => Successor_Link,
      Table_Index_Type     => Successor_Id,
      Table_Low_Bound      => Succ_First,
      Table_Initial        => 500,
      Table_Increment      => 200,
      Table_Name           => "Succ");

   --  For the case of Elaborate_All, the following table is used to record
   --  chains of with relationships that lead to the Elab_All link. These are
   --  used solely for diagnostic purposes

   type Elab_All_Entry is record
      Needed_By : Unit_Name_Type;
      --  Name of unit from which referencing unit was with'ed or otherwise
      --  needed as a result of Elaborate_All or Elaborate_Desirable.

      Next_Elab : Elab_All_Id;
      --  Link to next entry on chain (No_Elab_All_Link marks end of list)
   end record;

   package Elab_All_Entries is new Table.Table
     (Table_Component_Type => Elab_All_Entry,
      Table_Index_Type     => Elab_All_Id,
      Table_Low_Bound      => 1,
      Table_Initial        => 2000,
      Table_Increment      => 200,
      Table_Name           => "Elab_All_Entries");

   type Unit_Id_Array_Ptr is access Unit_Id_Array;

   --  A Unit_Node_Record is built for each active unit

   type Unit_Node_Record is record
      Successors : Successor_Id;
      --  Pointer to list of links for successor nodes

      Num_Pred : Int;
      --  Number of predecessors for this unit that have not yet been chosen.
      --  Normally non-negative, but can go negative in the case of units
      --  chosen by the diagnose error procedure (when cycles are being removed
      --  from the graph).

      Nextnp : Unit_Id;
      --  Forward pointer for list of units with no predecessors

      Visited : Boolean;
      --  Used in computing transitive closure for Elaborate_All and also in
      --  locating cycles and paths in the diagnose routines.

      Elab_Position : Nat;
      --  Initialized to zero. Set non-zero when a unit is chosen and placed in
      --  the elaboration order. The value represents the ordinal position in
      --  the elaboration order.

      --  The following are for Elab_New. We compute the strongly connected
      --  components (SCCs) of the directed graph of units. The edges are the
      --  Successors, which do not include pragmas Elaborate_All (explicit or
      --  implicit) in Elab_New. In addition, we assume there is a edge
      --  pointing from a body to its corresponding spec; this edge is not
      --  included in Successors, because of course a spec is elaborated BEFORE
      --  its body, not after.

      SCC_Root : Unit_Id;
      --  Each unit points to the root of its SCC, which is just an arbitrary
      --  member of the SCC. Two units are in the same SCC if and only if their
      --  SCC_Roots are equal. U is the root of its SCC if and only if
      --  SCC(U)=U.

      Nodes : Unit_Id_Array_Ptr;
      --  Present only in the root of an SCC. This is the set of units in the
      --  SCC, in no particular order.

      SCC_Num_Pred : Int;
      --  Present only in the root of an SCC. This is the number of predecessor
      --  units of the SCC that are in other SCCs, and that have not yet been
      --  chosen.

      Validate_Seen : Boolean := False;
      --  See procedure Validate below
   end record;

   package UNR is new Table.Table
     (Table_Component_Type => Unit_Node_Record,
      Table_Index_Type     => Unit_Id,
      Table_Low_Bound      => First_Unit_Entry,
      Table_Initial        => 500,
      Table_Increment      => 200,
      Table_Name           => "UNR");

   No_Pred : Unit_Id;
   --  Head of list of items with no predecessors

   Num_Left : Int;
   --  Number of entries not yet dealt with

   Cur_Unit : Unit_Id;
   --  Current unit, set by Gather_Dependencies, and picked up in Build_Link to
   --  set the Reason_Unit field of the created dependency link.

   Num_Chosen : Nat;
   --  Number of units chosen in the elaboration order so far

   Diagnose_Elaboration_Problem_Called : Boolean := False;
   --  True if Diagnose_Elaboration_Problem was called. Used in an assertion.

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Debug_Flag_Older return Boolean;
   function Debug_Flag_Old return Boolean;
   --  True if debug flags select the old or older algorithms. Pretty much any
   --  change to elaboration order can break some programs. For example,
   --  programs can depend on elaboration order even without failing
   --  access-before-elaboration checks. A trivial example is a program that
   --  prints text during elaboration. Therefore, we have flags to revert to
   --  the old(er) algorithms.

   procedure Validate (Order : Unit_Id_Array; Doing_New : Boolean);
   --  Assert that certain properties are true

   function Better_Choice_Optimistic
     (U1 : Unit_Id;
      U2 : Unit_Id) return Boolean;
   --  U1 and U2 are both permitted candidates for selection as the next unit
   --  to be elaborated. This function determines whether U1 is a better choice
   --  than U2, i.e. should be elaborated in preference to U2, based on a set
   --  of heuristics that establish a friendly and predictable order (see body
   --  for details). The result is True if U1 is a better choice than U2, and
   --  False if it is a worse choice, or there is no preference between them.

   function Better_Choice_Pessimistic
     (U1 : Unit_Id;
      U2 : Unit_Id) return Boolean;
   --  This is like Better_Choice_Optimistic, and has the same interface, but
   --  returns true if U1 is a worse choice than U2 in the sense of the -p
   --  (pessimistic elaboration order) switch. We still have to obey Ada rules,
   --  so it is not quite the direct inverse of Better_Choice_Optimistic.

   function Better_Choice (U1 : Unit_Id; U2 : Unit_Id) return Boolean;
   --  Calls Better_Choice_Optimistic or Better_Choice_Pessimistic as
   --  appropriate. Also takes care of the U2 = No_Unit_Id case.

   procedure Build_Link
     (Before : Unit_Id;
      After  : Unit_Id;
      R      : Succ_Reason;
      Ea_Id  : Elab_All_Id := No_Elab_All_Link);
   --  Establish a successor link, Before must be elaborated before After, and
   --  the reason for the link is R. Ea_Id is the contents to be placed in the
   --  Elab_All_Link of the entry.

   procedure Choose
     (Elab_Order : in out Unit_Id_Table;
      Chosen     : Unit_Id;
      Msg        : String);
   --  Chosen is the next entry chosen in the elaboration order. This procedure
   --  updates all data structures appropriately.

   function Corresponding_Body (U : Unit_Id) return Unit_Id;
   pragma Inline (Corresponding_Body);
   --  Given a unit that is a spec for which there is a separate body, return
   --  the unit id of the body. It is an error to call this routine with a unit
   --  that is not a spec, or that does not have a separate body.

   function Corresponding_Spec (U : Unit_Id) return Unit_Id;
   pragma Inline (Corresponding_Spec);
   --  Given a unit that is a body for which there is a separate spec, return
   --  the unit id of the spec. It is an error to call this routine with a unit
   --  that is not a body, or that does not have a separate spec.

   procedure Diagnose_Elaboration_Problem
     (Elab_Order : in out Unit_Id_Table);
   pragma No_Return (Diagnose_Elaboration_Problem);
   --  Called when no elaboration order can be found. Outputs an appropriate
   --  diagnosis of the problem, and then abandons the bind.

   procedure Elab_All_Links
     (Before : Unit_Id;
      After  : Unit_Id;
      Reason : Succ_Reason;
      Link   : Elab_All_Id);
   --  Used to compute the transitive closure of elaboration links for an
   --  Elaborate_All pragma (Reason = Elab_All) or for an indication of
   --  Elaborate_All_Desirable (Reason = Elab_All_Desirable). Unit After has a
   --  pragma Elaborate_All or the front end has determined that a reference
   --  probably requires Elaborate_All, and unit Before must be previously
   --  elaborated. First a link is built making sure that unit Before is
   --  elaborated before After, then a recursive call ensures that we also
   --  build links for any units needed by Before (i.e. these units must/should
   --  also be elaborated before After). Link is used to build a chain of
   --  Elab_All_Entries to explain the reason for a link. The value passed is
   --  the chain so far.

   procedure Elab_Error_Msg (S : Successor_Id);
   --  Given a successor link, outputs an error message of the form
   --  "$ must be elaborated before $ ..." where ... is the reason.

   procedure Force_Elab_Order;
   --  Gather dependencies from the forced-elaboration-order file (-f switch)

   procedure Gather_Dependencies;
   --  Compute dependencies, building the Succ and UNR tables

   procedure Init;
   --  Initialize global data structures in this package body

   function Is_Body_Unit (U : Unit_Id) return Boolean;
   pragma Inline (Is_Body_Unit);
   --  Determines if given unit is a body

   function Is_Pure_Or_Preelab_Unit (U : Unit_Id) return Boolean;
   --  Returns True if corresponding unit is Pure or Preelaborate. Includes
   --  dealing with testing flags on spec if it is given a body.

   function Is_Waiting_Body (U : Unit_Id) return Boolean;
   pragma Inline (Is_Waiting_Body);
   --  Determines if U is a waiting body, defined as a body that has
   --  not been elaborated, but whose spec has been elaborated.

   function Make_Elab_All_Entry
     (Unam : Unit_Name_Type;
      Link : Elab_All_Id) return Elab_All_Id;
   --  Make an Elab_All_Entries table entry with the given Unam and Link

   function Unit_Id_Of (Uname : Unit_Name_Type) return Unit_Id;
   --  This function uses the Info field set in the names table to obtain
   --  the unit Id of a unit, given its name id value.

   procedure Write_Closure (Order : Unit_Id_Array);
   --  Write the closure. This is for the -R and -Ra switches, "list closure
   --  display".

   procedure Write_Dependencies;
   --  Write out dependencies (called only if appropriate option is set)

   procedure Write_Elab_All_Chain (S : Successor_Id);
   --  If the reason for the link S is Elaborate_All or Elaborate_Desirable,
   --  then this routine will output the "needed by" explanation chain.

   procedure Write_Elab_Order (Order : Unit_Id_Array; Title : String);
   --  Display elaboration order. This is for the -l switch. Title is a heading
   --  to print; an empty string is passed to indicate Zero_Formatting.

   package Elab_New is

      --  Implementation of the new algorithm

      procedure Write_SCC (U : Unit_Id);
      --  Write the unit names of the units in the SCC in which U lives

      procedure Find_Elab_Order (Elab_Order : out Unit_Id_Table);

      Elab_Cycle_Found : Boolean := False;
      --  Set True if Find_Elab_Order found a cycle (usually an illegal pragma
      --  Elaborate_All, explicit or implicit).

      function SCC (U : Unit_Id) return Unit_Id;
      --  The root of the strongly connected component containing U

      function SCC_Num_Pred (U : Unit_Id) return Int;
      --  The SCC_Num_Pred of the SCC in which U lives

      function Nodes (U : Unit_Id) return Unit_Id_Array_Ptr;
      --  The nodes of the strongly connected component containing U

   end Elab_New;

   use Elab_New;

   package Elab_Old is

      --  Implementation of the old algorithm

      procedure Find_Elab_Order (Elab_Order : out Unit_Id_Table);

   end Elab_Old;

   --  Most of the code is shared between old and new; such code is outside
   --  packages Elab_Old and Elab_New.

   -------------------
   -- Better_Choice --
   -------------------

   function Better_Choice (U1 : Unit_Id; U2 : Unit_Id) return Boolean is
      pragma Assert (U1 /= No_Unit_Id);
   begin
      if U2 = No_Unit_Id then
         return True;
      end if;

      if Pessimistic_Elab_Order then
         return Better_Choice_Pessimistic (U1, U2);
      else
         return Better_Choice_Optimistic (U1, U2);
      end if;
   end Better_Choice;

   ------------------------------
   -- Better_Choice_Optimistic --
   ------------------------------

   function Better_Choice_Optimistic
     (U1 : Unit_Id;
      U2 : Unit_Id) return Boolean
   is
      UT1 : Unit_Record renames Units.Table (U1);
      UT2 : Unit_Record renames Units.Table (U2);

   begin
      if Debug_Flag_B then
         Write_Str ("Better_Choice_Optimistic (");
         Write_Unit_Name (UT1.Uname);
         Write_Str (", ");
         Write_Unit_Name (UT2.Uname);
         Write_Line (")");
      end if;

      --  Note: the checks here are applied in sequence, and the ordering is
      --  significant (i.e. the more important criteria are applied first).

      --  Prefer a waiting body to one that is not a waiting body

      if Is_Waiting_Body (U1) and then not Is_Waiting_Body (U2) then
         if Debug_Flag_B then
            Write_Line ("  True: u1 is waiting body, u2 is not");
         end if;

         return True;

      elsif Is_Waiting_Body (U2) and then not Is_Waiting_Body (U1) then
         if Debug_Flag_B then
            Write_Line ("  False: u2 is waiting body, u1 is not");
         end if;

         return False;

      --  Prefer a predefined unit to a non-predefined unit

      elsif UT1.Predefined and then not UT2.Predefined then
         if Debug_Flag_B then
            Write_Line ("  True: u1 is predefined, u2 is not");
         end if;

         return True;

      elsif UT2.Predefined and then not UT1.Predefined then
         if Debug_Flag_B then
            Write_Line ("  False: u2 is predefined, u1 is not");
         end if;

         return False;

      --  Prefer an internal unit to a non-internal unit

      elsif UT1.Internal and then not UT2.Internal then
         if Debug_Flag_B then
            Write_Line ("  True: u1 is internal, u2 is not");
         end if;
         return True;

      elsif UT2.Internal and then not UT1.Internal then
         if Debug_Flag_B then
            Write_Line ("  False: u2 is internal, u1 is not");
         end if;

         return False;

      --  Prefer a pure or preelaborated unit to one that is not. Pure should
      --  come before preelaborated.

      elsif Is_Pure_Or_Preelab_Unit (U1)
              and then not
            Is_Pure_Or_Preelab_Unit (U2)
      then
         if Debug_Flag_B then
            Write_Line ("  True: u1 is pure/preelab, u2 is not");
         end if;

         return True;

      elsif Is_Pure_Or_Preelab_Unit (U2)
              and then not
            Is_Pure_Or_Preelab_Unit (U1)
      then
         if Debug_Flag_B then
            Write_Line ("  False: u2 is pure/preelab, u1 is not");
         end if;

         return False;

      --  Prefer a body to a spec

      elsif Is_Body_Unit (U1) and then not Is_Body_Unit (U2) then
         if Debug_Flag_B then
            Write_Line ("  True: u1 is body, u2 is not");
         end if;

         return True;

      elsif Is_Body_Unit (U2) and then not Is_Body_Unit (U1) then
         if Debug_Flag_B then
            Write_Line ("  False: u2 is body, u1 is not");
         end if;

         return False;

      --  If both are waiting bodies, then prefer the one whose spec is more
      --  recently elaborated. Consider the following:

      --     spec of A
      --     spec of B
      --     body of A or B?

      --  The normal waiting body preference would have placed the body of A
      --  before the spec of B if it could. Since it could not, then it must be
      --  the case that A depends on B. It is therefore a good idea to put the
      --  body of B first.

      elsif Is_Waiting_Body (U1) and then Is_Waiting_Body (U2) then
         declare
            Result : constant Boolean :=
                       UNR.Table (Corresponding_Spec (U1)).Elab_Position >
                       UNR.Table (Corresponding_Spec (U2)).Elab_Position;
         begin
            if Debug_Flag_B then
               if Result then
                  Write_Line ("  True: based on waiting body elab positions");
               else
                  Write_Line ("  False: based on waiting body elab positions");
               end if;
            end if;

            return Result;
         end;
      end if;

      --  Remaining choice rules are disabled by Debug flag -do

      if not Debug_Flag_Older then

         --  The following deal with the case of specs that have been marked
         --  as Elaborate_Body_Desirable. We generally want to delay these
         --  specs as long as possible, so that the bodies have a better chance
         --  of being elaborated closer to the specs.

         --  If we have two units, one of which is a spec for which this flag
         --  is set, and the other is not, we prefer to delay the spec for
         --  which the flag is set.

         if not UT1.Elaborate_Body_Desirable
           and then UT2.Elaborate_Body_Desirable
         then
            if Debug_Flag_B then
               Write_Line ("  True: u1 is elab body desirable, u2 is not");
            end if;

            return True;

         elsif not UT2.Elaborate_Body_Desirable
           and then UT1.Elaborate_Body_Desirable
         then
            if Debug_Flag_B then
               Write_Line ("  False: u1 is elab body desirable, u2 is not");
            end if;

            return False;

            --  If we have two specs that are both marked as Elaborate_Body
            --  desirable, we prefer the one whose body is nearer to being able
            --  to be elaborated, based on the Num_Pred count. This helps to
            --  ensure bodies are as close to specs as possible.

         elsif UT1.Elaborate_Body_Desirable
           and then UT2.Elaborate_Body_Desirable
         then
            declare
               Result : constant Boolean :=
                          UNR.Table (Corresponding_Body (U1)).Num_Pred <
                          UNR.Table (Corresponding_Body (U2)).Num_Pred;
            begin
               if Debug_Flag_B then
                  if Result then
                     Write_Line ("  True based on Num_Pred compare");
                  else
                     Write_Line ("  False based on Num_Pred compare");
                  end if;
               end if;

               return Result;
            end;
         end if;
      end if;

      --  If we have two specs in the same SCC, choose the one whose body is
      --  closer to being ready.

      if Doing_New
        and then SCC (U1) = SCC (U2)
        and then Units.Table (U1).Utype = Is_Spec
        and then Units.Table (U2).Utype = Is_Spec
        and then UNR.Table (Corresponding_Body (U1)).Num_Pred /=
                 UNR.Table (Corresponding_Body (U2)).Num_Pred
      then
         if UNR.Table (Corresponding_Body (U1)).Num_Pred <
           UNR.Table (Corresponding_Body (U2)).Num_Pred
         then
            if Debug_Flag_B then
               Write_Str ("  True: same SCC; ");
               Write_Int (UNR.Table (Corresponding_Body (U1)).Num_Pred);
               Write_Str (" < ");
               Write_Int (UNR.Table (Corresponding_Body (U2)).Num_Pred);
               Write_Eol;
            end if;

            return True;
         else
            if Debug_Flag_B then
               Write_Str ("  False: same SCC; ");
               Write_Int (UNR.Table (Corresponding_Body (U1)).Num_Pred);
               Write_Str (" > ");
               Write_Int (UNR.Table (Corresponding_Body (U2)).Num_Pred);
               Write_Eol;
            end if;

            return False;
         end if;
      end if;

      --  If we fall through, it means that no preference rule applies, so we
      --  use alphabetical order to at least give a deterministic result.

      if Debug_Flag_B then
         Write_Line ("  choose on alpha order");
      end if;

      return Uname_Less (UT1.Uname, UT2.Uname);
   end Better_Choice_Optimistic;

   -------------------------------
   -- Better_Choice_Pessimistic --
   -------------------------------

   function Better_Choice_Pessimistic
     (U1 : Unit_Id;
      U2 : Unit_Id) return Boolean
   is
      UT1 : Unit_Record renames Units.Table (U1);
      UT2 : Unit_Record renames Units.Table (U2);

   begin
      if Debug_Flag_B then
         Write_Str ("Better_Choice_Pessimistic (");
         Write_Unit_Name (UT1.Uname);
         Write_Str (", ");
         Write_Unit_Name (UT2.Uname);
         Write_Line (")");
      end if;

      --  Note: the checks here are applied in sequence, and the ordering is
      --  significant (i.e. the more important criteria are applied first).

      --  If either unit is predefined or internal, then we use the normal
      --  Better_Choice_Optimistic rule, since we don't want to disturb the
      --  elaboration rules of the language with -p; same treatment for
      --  Pure/Preelab.

      --  Prefer a predefined unit to a non-predefined unit

      if UT1.Predefined and then not UT2.Predefined then
         if Debug_Flag_B then
            Write_Line ("  True: u1 is predefined, u2 is not");
         end if;

         return True;

      elsif UT2.Predefined and then not UT1.Predefined then
         if Debug_Flag_B then
            Write_Line ("  False: u2 is predefined, u1 is not");
         end if;

         return False;

      --  Prefer an internal unit to a non-internal unit

      elsif UT1.Internal and then not UT2.Internal then
         if Debug_Flag_B then
            Write_Line ("  True: u1 is internal, u2 is not");
         end if;

         return True;

      elsif UT2.Internal and then not UT1.Internal then
         if Debug_Flag_B then
            Write_Line ("  False: u2 is internal, u1 is not");
         end if;

         return False;

      --  Prefer a pure or preelaborated unit to one that is not

      elsif Is_Pure_Or_Preelab_Unit (U1)
              and then not
            Is_Pure_Or_Preelab_Unit (U2)
      then
         if Debug_Flag_B then
            Write_Line ("  True: u1 is pure/preelab, u2 is not");
         end if;

         return True;

      elsif Is_Pure_Or_Preelab_Unit (U2)
              and then not
            Is_Pure_Or_Preelab_Unit (U1)
      then
         if Debug_Flag_B then
            Write_Line ("  False: u2 is pure/preelab, u1 is not");
         end if;

         return False;

      --  Prefer anything else to a waiting body. We want to make bodies wait
      --  as long as possible, till we are forced to choose them.

      elsif Is_Waiting_Body (U1) and then not Is_Waiting_Body (U2) then
         if Debug_Flag_B then
            Write_Line ("  False: u1 is waiting body, u2 is not");
         end if;

         return False;

      elsif Is_Waiting_Body (U2) and then not Is_Waiting_Body (U1) then
         if Debug_Flag_B then
            Write_Line ("  True: u2 is waiting body, u1 is not");
         end if;

         return True;

      --  Prefer a spec to a body (this is mandatory)

      elsif Is_Body_Unit (U1) and then not Is_Body_Unit (U2) then
         if Debug_Flag_B then
            Write_Line ("  False: u1 is body, u2 is not");
         end if;

         return False;

      elsif Is_Body_Unit (U2) and then not Is_Body_Unit (U1) then
         if Debug_Flag_B then
            Write_Line ("  True: u2 is body, u1 is not");
         end if;

         return True;

      --  If both are waiting bodies, then prefer the one whose spec is less
      --  recently elaborated. Consider the following:

      --     spec of A
      --     spec of B
      --     body of A or B?

      --  The normal waiting body preference would have placed the body of A
      --  before the spec of B if it could. Since it could not, then it must be
      --  the case that A depends on B. It is therefore a good idea to put the
      --  body of B last so that if there is an elaboration order problem, we
      --  will find it (that's what pessimistic order is about).

      elsif Is_Waiting_Body (U1) and then Is_Waiting_Body (U2) then
         declare
            Result : constant Boolean :=
                       UNR.Table (Corresponding_Spec (U1)).Elab_Position <
                       UNR.Table (Corresponding_Spec (U2)).Elab_Position;
         begin
            if Debug_Flag_B then
               if Result then
                  Write_Line ("  True: based on waiting body elab positions");
               else
                  Write_Line ("  False: based on waiting body elab positions");
               end if;
            end if;

            return Result;
         end;
      end if;

      --  Remaining choice rules are disabled by Debug flag -do

      if not Debug_Flag_Older then

         --  The following deal with the case of specs that have been marked as
         --  Elaborate_Body_Desirable. In the normal case, we generally want to
         --  delay the elaboration of these specs as long as possible, so that
         --  bodies have better chance of being elaborated closer to the specs.
         --  Better_Choice_Pessimistic as usual wants to do the opposite and
         --  elaborate such specs as early as possible.

         --  If we have two units, one of which is a spec for which this flag
         --  is set, and the other is not, we normally prefer to delay the spec
         --  for which the flag is set, so again Better_Choice_Pessimistic does
         --  the opposite.

         if not UT1.Elaborate_Body_Desirable
           and then UT2.Elaborate_Body_Desirable
         then
            if Debug_Flag_B then
               Write_Line ("  False: u1 is elab body desirable, u2 is not");
            end if;

            return False;

         elsif not UT2.Elaborate_Body_Desirable
           and then UT1.Elaborate_Body_Desirable
         then
            if Debug_Flag_B then
               Write_Line ("  True: u1 is elab body desirable, u2 is not");
            end if;

            return True;

            --  If we have two specs that are both marked as Elaborate_Body
            --  desirable, we normally prefer the one whose body is nearer to
            --  being able to be elaborated, based on the Num_Pred count. This
            --  helps to ensure bodies are as close to specs as possible. As
            --  usual, Better_Choice_Pessimistic does the opposite.

         elsif UT1.Elaborate_Body_Desirable
           and then UT2.Elaborate_Body_Desirable
         then
            declare
               Result : constant Boolean :=
                          UNR.Table (Corresponding_Body (U1)).Num_Pred >=
                          UNR.Table (Corresponding_Body (U2)).Num_Pred;
            begin
               if Debug_Flag_B then
                  if Result then
                     Write_Line ("  True based on Num_Pred compare");
                  else
                     Write_Line ("  False based on Num_Pred compare");
                  end if;
               end if;

               return Result;
            end;
         end if;
      end if;

      --  If we fall through, it means that no preference rule applies, so we
      --  use alphabetical order to at least give a deterministic result. Since
      --  Better_Choice_Pessimistic is in the business of stirring up the
      --  order, we will use reverse alphabetical ordering.

      if Debug_Flag_B then
         Write_Line ("  choose on reverse alpha order");
      end if;

      return Uname_Less (UT2.Uname, UT1.Uname);
   end Better_Choice_Pessimistic;

   ----------------
   -- Build_Link --
   ----------------

   procedure Build_Link
     (Before : Unit_Id;
      After  : Unit_Id;
      R      : Succ_Reason;
      Ea_Id  : Elab_All_Id := No_Elab_All_Link)
   is
      Cspec : Unit_Id;

   begin
      Succ.Append
        ((Before        => Before,
          After         => No_Unit_Id, -- filled in below
          Next          => UNR.Table (Before).Successors,
          Reason        => R,
          Elab_Body     => False, -- set correctly below
          Reason_Unit   => Cur_Unit,
          Elab_All_Link => Ea_Id));
      UNR.Table (Before).Successors := Succ.Last;

      --  Deal with special Elab_Body case. If the After of this link is
      --  a body whose spec has Elaborate_All set, and this is not the link
      --  directly from the body to the spec, then we make the After of the
      --  link reference its spec instead, marking the link appropriately.

      if Units.Table (After).Utype = Is_Body then
         Cspec := Corresponding_Spec (After);

         if Units.Table (Cspec).Elaborate_Body
           and then Cspec /= Before
         then
            Succ.Table (Succ.Last).After     := Cspec;
            Succ.Table (Succ.Last).Elab_Body := True;
            UNR.Table (Cspec).Num_Pred       := UNR.Table (Cspec).Num_Pred + 1;
            return;
         end if;
      end if;

      --  Fall through on normal case

      Succ.Table (Succ.Last).After     := After;
      Succ.Table (Succ.Last).Elab_Body := False;
      UNR.Table (After).Num_Pred       := UNR.Table (After).Num_Pred + 1;
   end Build_Link;

   ------------
   -- Choose --
   ------------

   procedure Choose
     (Elab_Order : in out Unit_Id_Table;
      Chosen     : Unit_Id;
      Msg        : String)
   is
      pragma Assert (Chosen /= No_Unit_Id);
      S : Successor_Id;
      U : Unit_Id;

   begin
      if Debug_Flag_C then
         Write_Str ("Choosing Unit ");
         Write_Unit_Name (Units.Table (Chosen).Uname);
         Write_Str (Msg);
      end if;

      --  We shouldn't be choosing something with unelaborated predecessors,
      --  and we shouldn't call this twice on the same unit. But that's not
      --  true when this is called from Diagnose_Elaboration_Problem.

      if Errors_Detected = 0 then
         pragma Assert (UNR.Table (Chosen).Num_Pred = 0);
         pragma Assert (UNR.Table (Chosen).Elab_Position = 0);
         pragma Assert (not Doing_New or else SCC_Num_Pred (Chosen) = 0);
         null;
      end if;

      --  Add to elaboration order. Note that units having no elaboration code
      --  are not treated specially yet. The special casing of this is in
      --  Bindgen, where Gen_Elab_Calls skips over them. Meanwhile we need them
      --  here, because the object file list is also driven by the contents of
      --  the Elab_Order table.

      Append (Elab_Order, Chosen);

      --  Remove from No_Pred list. This is a little inefficient and may be we
      --  should doubly link the list, but it will do for now.

      if No_Pred = Chosen then
         No_Pred := UNR.Table (Chosen).Nextnp;
      else
         U := No_Pred;
         while U /= No_Unit_Id loop
            if UNR.Table (U).Nextnp = Chosen then
               UNR.Table (U).Nextnp := UNR.Table (Chosen).Nextnp;
               goto Done_Removal;
            end if;

            U := UNR.Table (U).Nextnp;
         end loop;

         --  Here if we didn't find it on the No_Pred list. This can happen
         --  only in calls from the Diagnose_Elaboration_Problem routine,
         --  where cycles are being removed arbitrarily from the graph.

         pragma Assert (Errors_Detected > 0);
         <<Done_Removal>> null;
      end if;

      --  For all successors, decrement the number of predecessors, and if it
      --  becomes zero, then add to no-predecessor list.

      S := UNR.Table (Chosen).Successors;
      pragma Annotate (CodePeer, Modified, S);

      while S /= No_Successor loop
         U := Succ.Table (S).After;
         UNR.Table (U).Num_Pred := UNR.Table (U).Num_Pred - 1;

         if Debug_Flag_N then
            Write_Str ("  decrementing Num_Pred for unit ");
            Write_Unit_Name (Units.Table (U).Uname);
            Write_Str (" new value = ");
            Write_Int (UNR.Table (U).Num_Pred);
            Write_Eol;
         end if;

         if UNR.Table (U).Num_Pred = 0 then
            UNR.Table (U).Nextnp := No_Pred;
            No_Pred := U;
         end if;

         if Doing_New and then SCC (U) /= SCC (Chosen) then
            UNR.Table (SCC (U)).SCC_Num_Pred :=
              UNR.Table (SCC (U)).SCC_Num_Pred - 1;

            if Debug_Flag_N then
               Write_Str ("  decrementing SCC_Num_Pred for unit ");
               Write_Unit_Name (Units.Table (U).Uname);
               Write_Str (" new value = ");
               Write_Int (SCC_Num_Pred (U));
               Write_Eol;
            end if;
         end if;

         S := Succ.Table (S).Next;
      end loop;

      --  All done, adjust number of units left count and set elaboration pos

      Num_Left   := Num_Left   - 1;
      Num_Chosen := Num_Chosen + 1;

      pragma Assert
        (Errors_Detected > 0 or else Num_Chosen = Last (Elab_Order));
      pragma Assert (Units.Last = UNR.Last);
      pragma Assert (Num_Chosen + Num_Left = Int (UNR.Last));

      if Debug_Flag_C then
         Write_Str (" ");
         Write_Int (Int (Num_Chosen));
         Write_Str ("+");
         Write_Int (Num_Left);
         Write_Str ("=");
         Write_Int (Int (UNR.Last));
         Write_Eol;
      end if;

      UNR.Table (Chosen).Elab_Position := Num_Chosen;

      --  If we just chose a spec with Elaborate_Body set, then we must
      --  immediately elaborate the body, before any other units.

      if Units.Table (Chosen).Elaborate_Body then

         --  If the unit is a spec only, then there is no body. This is a bit
         --  odd given that Elaborate_Body is here, but it is valid in an RCI
         --  unit, where we only have the interface in the stub bind.

         if Units.Table (Chosen).Utype = Is_Spec_Only
           and then Units.Table (Chosen).RCI
         then
            null;

         --  If this unit is an interface to a stand-alone library, then we
         --  don't want to elaborate the body -- that will happen as part of
         --  the library.

         elsif Units.Table (Chosen).SAL_Interface then
            null;

         else
            Choose
              (Elab_Order => Elab_Order,
               Chosen     => Corresponding_Body (Chosen),
               Msg        => " [Elaborate_Body]");
         end if;
      end if;
   end Choose;

   ------------------------
   -- Corresponding_Body --
   ------------------------

   --  Currently if the body and spec are separate, then they appear as two
   --  separate units in the same ALI file, with the body appearing first and
   --  the spec appearing second.

   function Corresponding_Body (U : Unit_Id) return Unit_Id is
   begin
      pragma Assert (Units.Table (U).Utype = Is_Spec);
      return U - 1;
   end Corresponding_Body;

   ------------------------
   -- Corresponding_Spec --
   ------------------------

   --  Currently if the body and spec are separate, then they appear as two
   --  separate units in the same ALI file, with the body appearing first and
   --  the spec appearing second.

   function Corresponding_Spec (U : Unit_Id) return Unit_Id is
   begin
      pragma Assert (Units.Table (U).Utype = Is_Body);
      return U + 1;
   end Corresponding_Spec;

   --------------------
   -- Debug_Flag_Old --
   --------------------

   function Debug_Flag_Old return Boolean is
   begin
      --  If the user specified both flags, we want to use the older algorithm,
      --  rather than some confusing mix of the two.

      return Debug_Flag_P and not Debug_Flag_O;
   end Debug_Flag_Old;

   ----------------------
   -- Debug_Flag_Older --
   ----------------------

   function Debug_Flag_Older return Boolean is
   begin
      return Debug_Flag_O;
   end Debug_Flag_Older;

   ----------------------------------
   -- Diagnose_Elaboration_Problem --
   ----------------------------------

   procedure Diagnose_Elaboration_Problem
     (Elab_Order : in out Unit_Id_Table)
   is
      function Find_Path
        (Ufrom : Unit_Id;
         Uto   : Unit_Id;
         ML    : Nat) return Boolean;
      --  Recursive routine used to find a path from node Ufrom to node Uto.
      --  If a path exists, returns True and outputs an appropriate set of
      --  error messages giving the path. Also calls Choose for each of the
      --  nodes so that they get removed from the remaining set. There are
      --  two cases of calls, either Ufrom = Uto for an attempt to find a
      --  cycle, or Ufrom is a spec and Uto the corresponding body for the
      --  case of an unsatisfiable Elaborate_Body pragma. ML is the minimum
      --  acceptable length for a path.

      ---------------
      -- Find_Path --
      ---------------

      function Find_Path
        (Ufrom : Unit_Id;
         Uto   : Unit_Id;
         ML    : Nat) return Boolean
      is
         function Find_Link (U : Unit_Id; PL : Nat) return Boolean;
         --  This is the inner recursive routine, it determines if a path
         --  exists from U to Uto, and if so returns True and outputs the
         --  appropriate set of error messages. PL is the path length

         ---------------
         -- Find_Link --
         ---------------

         function Find_Link (U : Unit_Id; PL : Nat) return Boolean is
            S : Successor_Id;

         begin
            --  Recursion ends if we are at terminating node and the path is
            --  sufficiently long, generate error message and return True.

            if U = Uto and then PL >= ML then
               Choose (Elab_Order, U, " [Find_Link: base]");
               return True;

            --  All done if already visited

            elsif UNR.Table (U).Visited then
               return False;

            --  Otherwise mark as visited and look at all successors

            else
               UNR.Table (U).Visited := True;

               S := UNR.Table (U).Successors;
               while S /= No_Successor loop
                  if Find_Link (Succ.Table (S).After, PL + 1) then
                     Elab_Error_Msg (S);
                     Choose (Elab_Order, U, " [Find_Link: recursive]");
                     return True;
                  end if;

                  S := Succ.Table (S).Next;
               end loop;

               --  Falling through means this does not lead to a path

               return False;
            end if;
         end Find_Link;

      --  Start of processing for Find_Path

      begin
         --  Initialize all non-chosen nodes to not visited yet

         for U in Units.First .. Units.Last loop
            UNR.Table (U).Visited := UNR.Table (U).Elab_Position /= 0;
         end loop;

         --  Now try to find the path

         return Find_Link (Ufrom, 0);
      end Find_Path;

   --  Start of processing for Diagnose_Elaboration_Problem

   begin
      Diagnose_Elaboration_Problem_Called := True;
      Set_Standard_Error;

      --  Output state of things if debug flag N set

      if Debug_Flag_N then
         declare
            NP : Int;

         begin
            Write_Eol;
            Write_Eol;
            Write_Line ("Diagnose_Elaboration_Problem called");
            Write_Line ("List of remaining unchosen units and predecessors");

            for U in Units.First .. Units.Last loop
               if UNR.Table (U).Elab_Position = 0 then
                  NP := UNR.Table (U).Num_Pred;
                  Write_Eol;
                  Write_Str ("  Unchosen unit: #");
                  Write_Int (Int (U));
                  Write_Str ("  ");
                  Write_Unit_Name (Units.Table (U).Uname);
                  Write_Str (" (Num_Pred = ");
                  Write_Int (NP);
                  Write_Line (")");

                  if NP = 0 then
                     if Units.Table (U).Elaborate_Body then
                        Write_Line
                          ("    (not chosen because of Elaborate_Body)");
                     else
                        Write_Line ("  ****************** why not chosen?");
                     end if;
                  end if;

                  --  Search links list to find unchosen predecessors

                  for S in Succ.First .. Succ.Last loop
                     declare
                        SL : Successor_Link renames Succ.Table (S);

                     begin
                        if SL.After = U
                          and then UNR.Table (SL.Before).Elab_Position = 0
                        then
                           Write_Str ("    unchosen predecessor: #");
                           Write_Int (Int (SL.Before));
                           Write_Str ("  ");
                           Write_Unit_Name (Units.Table (SL.Before).Uname);
                           Write_Eol;
                           NP := NP - 1;
                        end if;
                     end;
                  end loop;

                  if NP /= 0 then
                     Write_Line ("  **************** Num_Pred value wrong!");
                  end if;
               end if;
            end loop;
         end;
      end if;

      --  Output the header for the error, and manually increment the error
      --  count. We are using Error_Msg_Output rather than Error_Msg here for
      --  two reasons:

      --    This is really only one error, not one for each line
      --    We want this output on standard output since it is voluminous

      --  But we do need to deal with the error count manually in this case

      Errors_Detected := Errors_Detected + 1;
      Error_Msg_Output ("elaboration circularity detected", Info => False);

      --  Try to find cycles starting with any of the remaining nodes that have
      --  not yet been chosen. There must be at least one (there is some reason
      --  we are being called).

      for U in Units.First .. Units.Last loop
         if UNR.Table (U).Elab_Position = 0 then
            if Find_Path (U, U, 1) then
               raise Unrecoverable_Error;
            end if;
         end if;
      end loop;

      --  We should never get here, since we were called for some reason, and
      --  we should have found and eliminated at least one bad path.

      raise Program_Error;
   end Diagnose_Elaboration_Problem;

   --------------------
   -- Elab_All_Links --
   --------------------

   procedure Elab_All_Links
     (Before : Unit_Id;
      After  : Unit_Id;
      Reason : Succ_Reason;
      Link   : Elab_All_Id)
   is
   begin
      if UNR.Table (Before).Visited then
         return;
      end if;

      --  Build the direct link for Before

      UNR.Table (Before).Visited := True;
      Build_Link (Before, After, Reason, Link);

      --  Process all units with'ed by Before recursively

      for W in Units.Table (Before).First_With ..
               Units.Table (Before).Last_With
      loop
         --  Skip if this with is an interface to a stand-alone library. Skip
         --  also if no ALI file for this WITH, happens for language defined
         --  generics while bootstrapping the compiler (see body of routine
         --  Lib.Writ.Write_With_Lines). Finally, skip if it is a limited with
         --  clause, which does not impose an elaboration link.

         if not Withs.Table (W).SAL_Interface
           and then Withs.Table (W).Afile /= No_File
           and then not Withs.Table (W).Limited_With
         then
            declare
               Info : constant Int :=
                 Get_Name_Table_Int (Withs.Table (W).Uname);

            begin
               --  If the unit is unknown, for some unknown reason, fail
               --  graciously explaining that the unit is unknown. Without
               --  this check, gnatbind will crash in Unit_Id_Of.

               if Info = 0 or else Unit_Id (Info) = No_Unit_Id then
                  declare
                     Withed       : String  :=
                                      Get_Name_String (Withs.Table (W).Uname);
                     Last_Withed  : Natural := Withed'Last;
                     Withing      : String  :=
                                      Get_Name_String
                                        (Units.Table (Before).Uname);
                     Last_Withing : Natural := Withing'Last;
                     Spec_Body    : String  := " (Spec)";

                  begin
                     To_Mixed (Withed);
                     To_Mixed (Withing);

                     if Last_Withed > 2
                       and then Withed (Last_Withed - 1) = '%'
                     then
                        Last_Withed := Last_Withed - 2;
                     end if;

                     if Last_Withing > 2
                       and then Withing (Last_Withing - 1) = '%'
                     then
                        Last_Withing := Last_Withing - 2;
                     end if;

                     if Units.Table (Before).Utype = Is_Body
                       or else Units.Table (Before).Utype = Is_Body_Only
                     then
                        Spec_Body := " (Body)";
                     end if;

                     Osint.Fail
                       ("could not find unit "
                        & Withed (Withed'First .. Last_Withed) & " needed by "
                        & Withing (Withing'First .. Last_Withing) & Spec_Body);
                  end;
               end if;

               Elab_All_Links
                 (Unit_Id_Of (Withs.Table (W).Uname),
                  After,
                  Reason,
                  Make_Elab_All_Entry (Withs.Table (W).Uname, Link));
            end;
         end if;
      end loop;

      --  Process corresponding body, if there is one

      if Units.Table (Before).Utype = Is_Spec then
         Elab_All_Links
           (Corresponding_Body (Before),
            After, Reason,
            Make_Elab_All_Entry
              (Units.Table (Corresponding_Body (Before)).Uname, Link));
      end if;
   end Elab_All_Links;

   --------------------
   -- Elab_Error_Msg --
   --------------------

   procedure Elab_Error_Msg (S : Successor_Id) is
      SL : Successor_Link renames Succ.Table (S);

   begin
      --  Nothing to do if internal unit involved and no -da flag

      if not Debug_Flag_A
        and then
          (Is_Internal_File_Name (Units.Table (SL.Before).Sfile)
            or else
           Is_Internal_File_Name (Units.Table (SL.After).Sfile))
      then
         return;
      end if;

      --  Here we want to generate output

      Error_Msg_Unit_1 := Units.Table (SL.Before).Uname;

      if SL.Elab_Body then
         Error_Msg_Unit_2 := Units.Table (Corresponding_Body (SL.After)).Uname;
      else
         Error_Msg_Unit_2 := Units.Table (SL.After).Uname;
      end if;

      Error_Msg_Output ("  $ must be elaborated before $", Info => True);

      Error_Msg_Unit_1 := Units.Table (SL.Reason_Unit).Uname;

      case SL.Reason is
         when Withed =>
            Error_Msg_Output
              ("     reason: with clause",
               Info => True);

         when Forced =>
            Error_Msg_Output
              ("     reason: forced by -f switch",
               Info => True);

         when Elab =>
            Error_Msg_Output
              ("     reason: pragma Elaborate in unit $",
               Info => True);

         when Elab_All =>
            Error_Msg_Output
              ("     reason: pragma Elaborate_All in unit $",
               Info => True);

         when Elab_All_Desirable =>
            Error_Msg_Output
              ("     reason: implicit Elaborate_All in unit $",
               Info => True);

            Error_Msg_Output
              ("     recompile $ with -gnatel for full details",
               Info => True);

         when Elab_Desirable =>
            Error_Msg_Output
              ("     reason: implicit Elaborate in unit $",
               Info => True);

            Error_Msg_Output
              ("     recompile $ with -gnatel for full details",
               Info => True);

         when Spec_First =>
            Error_Msg_Output
              ("     reason: spec always elaborated before body",
               Info => True);
      end case;

      Write_Elab_All_Chain (S);

      if SL.Elab_Body then
         Error_Msg_Unit_1 := Units.Table (SL.Before).Uname;
         Error_Msg_Unit_2 := Units.Table (SL.After).Uname;
         Error_Msg_Output
           ("  $ must therefore be elaborated before $", True);

         Error_Msg_Unit_1 := Units.Table (SL.After).Uname;
         Error_Msg_Output
           ("     (because $ has a pragma Elaborate_Body)", True);
      end if;

      if not Zero_Formatting then
         Write_Eol;
      end if;
   end Elab_Error_Msg;

   ---------------------
   -- Find_Elab_Order --
   ---------------------

   procedure Find_Elab_Order
     (Elab_Order          : out Unit_Id_Table;
      First_Main_Lib_File : File_Name_Type)
   is
      function Num_Spec_Body_Pairs (Order : Unit_Id_Array) return Nat;
      --  Number of cases where the body of a unit immediately follows the
      --  corresponding spec. Such cases are good, because calls to that unit
      --  from outside can't get ABE.

      -------------------------
      -- Num_Spec_Body_Pairs --
      -------------------------

      function Num_Spec_Body_Pairs (Order : Unit_Id_Array) return Nat is
         Result : Nat := 0;

      begin
         for J in Order'First + 1 .. Order'Last loop
            if Units.Table (Order (J - 1)).Utype = Is_Spec
              and then Units.Table (Order (J)).Utype = Is_Body
              and then Corresponding_Spec (Order (J)) = Order (J - 1)
            then
               Result := Result + 1;
            end if;
         end loop;

         return Result;
      end Num_Spec_Body_Pairs;

      --  Local variables

      Old_Elab_Order : Unit_Id_Table;

   --  Start of processing for Find_Elab_Order

   begin
      --  Output warning if -p used with no -gnatE units

      if Pessimistic_Elab_Order
        and not Dynamic_Elaboration_Checks_Specified
      then
         Error_Msg ("?use of -p switch questionable");
         Error_Msg ("?since all units compiled with static elaboration model");
      end if;

      if Do_New and not Debug_Flag_Old and not Debug_Flag_Older then
         if Debug_Flag_V then
            Write_Line ("Doing new...");
         end if;

         Doing_New := True;
         Init;
         Elab_New.Find_Elab_Order (Elab_Order);
      end if;

      --  Elab_New does not support the pessimistic order, so if that was
      --  requested, use the old results. Use Elab_Old if -dp or -do was
      --  selected. Elab_New does not yet give proper error messages for
      --  illegal Elaborate_Alls, so if there is one, run Elab_Old.

      if Do_Old
        or Pessimistic_Elab_Order
        or Debug_Flag_Old
        or Debug_Flag_Older
        or Elab_Cycle_Found
      then
         if Debug_Flag_V then
            Write_Line ("Doing old...");
         end if;

         Doing_New := False;
         Init;
         Elab_Old.Find_Elab_Order (Old_Elab_Order);
      end if;

      pragma Assert (Elab_Cycle_Found <= -- implies
                       Diagnose_Elaboration_Problem_Called);

      declare
         Old_Order : Unit_Id_Array renames
                       Old_Elab_Order.Table (1 .. Last (Old_Elab_Order));
      begin
         if Do_Old and Do_New then
            declare
               New_Order : Unit_Id_Array renames
                             Elab_Order.Table (1 .. Last (Elab_Order));
               Old_Pairs : constant Nat := Num_Spec_Body_Pairs (Old_Order);
               New_Pairs : constant Nat := Num_Spec_Body_Pairs (New_Order);

            begin
               Write_Line (Get_Name_String (First_Main_Lib_File));

               pragma Assert (Old_Order'Length = New_Order'Length);
               pragma Debug (Validate (Old_Order, Doing_New => False));
               pragma Debug (Validate (New_Order, Doing_New => True));

               --  Misc debug printouts that can be used for experimentation by
               --  changing the 'if's below.

               if True then
                  if New_Order = Old_Order then
                     Write_Line ("Elab_New: same order.");
                  else
                     Write_Line ("Elab_New: diff order.");
                  end if;
               end if;

               if New_Order /= Old_Order and then False then
                  Write_Line ("Elaboration orders differ:");
                  Write_Elab_Order
                    (Old_Order, Title => "OLD ELABORATION ORDER");
                  Write_Elab_Order
                    (New_Order, Title => "NEW ELABORATION ORDER");
               end if;

               if True then
                  Write_Str ("Pairs: ");
                  Write_Int (Old_Pairs);

                  if Old_Pairs = New_Pairs then
                     Write_Str (" = ");
                  elsif Old_Pairs < New_Pairs then
                     Write_Str (" < ");
                  else
                     Write_Str (" > ");
                  end if;

                  Write_Int (New_Pairs);
                  Write_Eol;
               end if;

               if Old_Pairs /= New_Pairs and then False then
                  Write_Str ("Pairs: ");
                  Write_Int (Old_Pairs);

                  if Old_Pairs < New_Pairs then
                     Write_Str (" < ");
                  else
                     Write_Str (" > ");
                  end if;

                  Write_Int (New_Pairs);
                  Write_Eol;

                  if Old_Pairs /= New_Pairs and then Debug_Flag_V then
                     Write_Elab_Order
                       (Old_Order, Title => "OLD ELABORATION ORDER");
                     Write_Elab_Order
                       (New_Order, Title => "NEW ELABORATION ORDER");
                     pragma Assert (New_Pairs >= Old_Pairs);
                  end if;
               end if;
            end;
         end if;

         --  The Elab_New algorithm doesn't implement the -p switch, so if that
         --  was used, use the results from the old algorithm. Likewise if the
         --  user has requested the old algorithm.

         if Pessimistic_Elab_Order or Debug_Flag_Old or Debug_Flag_Older then
            pragma Assert
              (Last (Elab_Order) = 0
                or else Last (Elab_Order) = Old_Order'Last);

            Init (Elab_Order);
            Append_All (Elab_Order, Old_Order);
         end if;

         --  Now set the Elab_Positions in the Units table. It is important to
         --  do this late, in case we're running both Elab_New and Elab_Old.

         declare
            New_Order : Unit_Id_Array renames
                          Elab_Order.Table (1 .. Last (Elab_Order));
            Units_Array : Units.Table_Type renames
                            Units.Table (Units.First .. Units.Last);
         begin
            for J in New_Order'Range loop
               pragma Assert
                 (UNR.Table (New_Order (J)).Elab_Position = J);
               Units_Array  (New_Order (J)).Elab_Position := J;
            end loop;

            if Errors_Detected = 0 then

               --  Display elaboration order if -l was specified

               if Elab_Order_Output then
                  if Zero_Formatting then
                     Write_Elab_Order (New_Order, Title => "");
                  else
                     Write_Elab_Order
                       (New_Order, Title => "ELABORATION ORDER");
                  end if;
               end if;

               --  Display list of sources in the closure (except predefined
               --  sources) if -R was used. Include predefined sources if -Ra
               --  was used.

               if List_Closure then
                  Write_Closure (New_Order);
               end if;
            end if;
         end;
      end;
   end Find_Elab_Order;

   ----------------------
   -- Force_Elab_Order --
   ----------------------

   procedure Force_Elab_Order is
      subtype Header_Num is Unit_Name_Type'Base range 0 .. 2**16 - 1;

      function Hash (N : Unit_Name_Type) return Header_Num;

      package Name_Map is new System.HTable.Simple_HTable
        (Header_Num => Header_Num,
         Element    => Logical_Line_Number,
         No_Element => No_Line_Number,
         Key        => Unit_Name_Type,
         Hash       => Hash,
         Equal      => "=");
      --  Name_Map contains an entry for each file name seen, mapped to the
      --  line number where we saw it first. This is used to give an error for
      --  duplicates.

      ----------
      -- Hash --
      ----------

      function Hash (N : Unit_Name_Type) return Header_Num is
         --  Name_Ids are already widely dispersed; no need for any actual
         --  hashing. Just subtract to make it zero based, and "mod" to
         --  bring it in range.
      begin
         return (N - Unit_Name_Type'First) mod (Header_Num'Last + 1);
      end Hash;

      --  Local variables

      Cur_Line_Number : Logical_Line_Number;
      Error           : Boolean := False;
      Iter            : Forced_Units_Iterator;
      Prev_Unit       : Unit_Id := No_Unit_Id;
      Uname           : Unit_Name_Type;

   --  Start of processing for Force_Elab_Order

   begin
      Iter := Iterate_Forced_Units;
      while Has_Next (Iter) loop
         Next (Iter, Uname, Cur_Line_Number);

         declare
            Dup : constant Logical_Line_Number := Name_Map.Get (Uname);
         begin
            if Dup = No_Line_Number then
               Name_Map.Set (Uname, Cur_Line_Number);

               --  We don't need to give the "not present" message in the case
               --  of "duplicate unit", because we would have already given the
               --  "not present" message on the first occurrence.

               if Get_Name_Table_Int (Uname) = 0
                 or else Unit_Id (Get_Name_Table_Int (Uname)) = No_Unit_Id
               then
                  Error := True;
                  if Doing_New then
                     Write_Line
                       ("""" & Get_Name_String (Uname)
                        & """: not present; ignored");
                  end if;
               end if;

            else
               Error := True;
               if Doing_New then
                  Error_Msg_Nat_1  := Nat (Cur_Line_Number);
                  Error_Msg_Unit_1 := Uname;
                  Error_Msg_Nat_2  := Nat (Dup);
                  Error_Msg
                    (Force_Elab_Order_File.all
                     & ":#: duplicate unit name $ from line #");
               end if;
            end if;
         end;

         if not Error then
            declare
               Cur_Unit : constant Unit_Id := Unit_Id_Of (Uname);
            begin
               if Is_Internal_File_Name (Units.Table (Cur_Unit).Sfile) then
                  if Doing_New then
                     Write_Line
                       ("""" & Get_Name_String (Uname)
                        & """: predefined unit ignored");
                  end if;

               else
                  if Prev_Unit /= No_Unit_Id then
                     if Doing_New then
                        Write_Unit_Name (Units.Table (Prev_Unit).Uname);
                        Write_Str (" <-- ");
                        Write_Unit_Name (Units.Table (Cur_Unit).Uname);
                        Write_Eol;
                     end if;

                     Build_Link
                       (Before => Prev_Unit,
                        After  => Cur_Unit,
                        R      => Forced);
                  end if;

                  Prev_Unit := Cur_Unit;
               end if;
            end;
         end if;
      end loop;
   end Force_Elab_Order;

   -------------------------
   -- Gather_Dependencies --
   -------------------------

   procedure Gather_Dependencies is
      Withed_Unit : Unit_Id;

   begin
      --  Loop through all units

      for U in Units.First .. Units.Last loop
         Cur_Unit := U;

         --  If this is not an interface to a stand-alone library and there is
         --  a body and a spec, then spec must be elaborated first. Note that
         --  the corresponding spec immediately follows the body.

         if not Units.Table (U).SAL_Interface
           and then Units.Table (U).Utype = Is_Body
         then
            Build_Link (Corresponding_Spec (U), U, Spec_First);
         end if;

         --  If this unit is not an interface to a stand-alone library, process
         --  WITH references for this unit ignoring interfaces to stand-alone
         --  libraries.

         if not Units.Table (U).SAL_Interface then
            for W in Units.Table (U).First_With ..
                     Units.Table (U).Last_With
            loop
               if Withs.Table (W).Sfile /= No_File
                 and then (not Withs.Table (W).SAL_Interface)
               then
                  --  Check for special case of withing a unit that does not
                  --  exist any more. If the unit was completely missing we
                  --  would already have detected this, but a nasty case arises
                  --  when we have a subprogram body with no spec, and some
                  --  obsolete unit with's a previous (now disappeared) spec.

                  if Get_Name_Table_Int (Withs.Table (W).Uname) = 0 then
                     if Doing_New then
                        Error_Msg_File_1 := Units.Table (U).Sfile;
                        Error_Msg_Unit_1 := Withs.Table (W).Uname;
                        Error_Msg ("{ depends on $ which no longer exists");
                     end if;

                     goto Next_With;
                  end if;

                  Withed_Unit := Unit_Id_Of (Withs.Table (W).Uname);

                  --  Pragma Elaborate_All case, for this we use the recursive
                  --  Elab_All_Links procedure to establish the links.

                  --  Elab_New ignores Elaborate_All and Elab_All_Desirable,
                  --  except for error messages.

                  if Withs.Table (W).Elaborate_All and then not Doing_New then

                     --  Reset flags used to stop multiple visits to a given
                     --  node.

                     for Uref in UNR.First .. UNR.Last loop
                        UNR.Table (Uref).Visited := False;
                     end loop;

                     --  Now establish all the links we need

                     Elab_All_Links
                       (Withed_Unit, U, Elab_All,
                        Make_Elab_All_Entry
                          (Withs.Table (W).Uname, No_Elab_All_Link));

                  --  Elaborate_All_Desirable case, for this we establish the
                  --  same links as above, but with a different reason.

                  elsif Withs.Table (W).Elab_All_Desirable
                    and then not Doing_New
                  then
                     --  Reset flags used to stop multiple visits to a given
                     --  node.

                     for Uref in UNR.First .. UNR.Last loop
                        UNR.Table (Uref).Visited := False;
                     end loop;

                     --  Now establish all the links we need

                     Elab_All_Links
                       (Withed_Unit, U, Elab_All_Desirable,
                        Make_Elab_All_Entry
                          (Withs.Table (W).Uname, No_Elab_All_Link));

                  --  Pragma Elaborate case. We must build a link for the
                  --  withed unit itself, and also the corresponding body if
                  --  there is one.

                  --  However, skip this processing if there is no ALI file for
                  --  the WITH entry, because this means it is a generic (even
                  --  when we fix the generics so that an ALI file is present,
                  --  we probably still will have no ALI file for unchecked and
                  --  other special cases).

                  elsif Withs.Table (W).Elaborate
                    and then Withs.Table (W).Afile /= No_File
                  then
                     Build_Link (Withed_Unit, U, Withed);

                     if Units.Table (Withed_Unit).Utype = Is_Spec then
                        Build_Link
                          (Corresponding_Body (Withed_Unit), U, Elab);
                     end if;

                  --  Elaborate_Desirable case, for this we establish the same
                  --  links as above, but with a different reason.

                  elsif Withs.Table (W).Elab_Desirable then
                     Build_Link (Withed_Unit, U, Withed);

                     if Units.Table (Withed_Unit).Utype = Is_Spec then
                        Build_Link
                          (Corresponding_Body (Withed_Unit),
                           U, Elab_Desirable);
                     end if;

                  --  A limited_with does not establish an elaboration
                  --  dependence (that's the whole point).

                  elsif Withs.Table (W).Limited_With then
                     null;

                  --  Case of normal WITH with no elaboration pragmas, just
                  --  build the single link to the directly referenced unit

                  else
                     Build_Link (Withed_Unit, U, Withed);
                  end if;
               end if;

               <<Next_With>>
               null;
            end loop;
         end if;
      end loop;

      --  If -f<elab_order> switch was given, take into account dependences
      --  specified in the file <elab_order>.

      if Force_Elab_Order_File /= null then
         Force_Elab_Order;
      end if;

      --  Output elaboration dependencies if option is set

      if Elab_Dependency_Output or Debug_Flag_E then
         if Doing_New then
            Write_Dependencies;
         end if;
      end if;
   end Gather_Dependencies;

   ----------
   -- Init --
   ----------

   procedure Init is
   begin
      Num_Chosen := 0;
      Num_Left := Int (Units.Last - Units.First + 1);
      Succ.Init;
      Elab_All_Entries.Init;
      UNR.Init;

      --  Initialize unit table for elaboration control

      for U in Units.First .. Units.Last loop
         UNR.Append
           ((Successors    => No_Successor,
             Num_Pred      => 0,
             Nextnp        => No_Unit_Id,
             Visited       => False,
             Elab_Position => 0,
             SCC_Root      => No_Unit_Id,
             Nodes         => null,
             SCC_Num_Pred  => 0,
             Validate_Seen => False));
      end loop;
   end Init;

   ------------------
   -- Is_Body_Unit --
   ------------------

   function Is_Body_Unit (U : Unit_Id) return Boolean is
   begin
      return
        Units.Table (U).Utype = Is_Body
          or else Units.Table (U).Utype = Is_Body_Only;
   end Is_Body_Unit;

   -----------------------------
   -- Is_Pure_Or_Preelab_Unit --
   -----------------------------

   function Is_Pure_Or_Preelab_Unit (U : Unit_Id) return Boolean is
   begin
      --  If we have a body with separate spec, test flags on the spec

      if Units.Table (U).Utype = Is_Body then
         return
           Units.Table (Corresponding_Spec (U)).Preelab
             or else Units.Table (Corresponding_Spec (U)).Pure;

      --  Otherwise we have a spec or body acting as spec, test flags on unit

      else
         return Units.Table (U).Preelab or else Units.Table (U).Pure;
      end if;
   end Is_Pure_Or_Preelab_Unit;

   ---------------------
   -- Is_Waiting_Body --
   ---------------------

   function Is_Waiting_Body (U : Unit_Id) return Boolean is
   begin
      return
        Units.Table (U).Utype = Is_Body
          and then UNR.Table (Corresponding_Spec (U)).Elab_Position /= 0;
   end Is_Waiting_Body;

   -------------------------
   -- Make_Elab_All_Entry --
   -------------------------

   function Make_Elab_All_Entry
     (Unam : Unit_Name_Type;
      Link : Elab_All_Id) return Elab_All_Id
   is
   begin
      Elab_All_Entries.Append ((Needed_By => Unam, Next_Elab => Link));
      return Elab_All_Entries.Last;
   end Make_Elab_All_Entry;

   ----------------
   -- Unit_Id_Of --
   ----------------

   function Unit_Id_Of (Uname : Unit_Name_Type) return Unit_Id is
      Info : constant Int := Get_Name_Table_Int (Uname);

   begin
      pragma Assert (Info /= 0 and then Unit_Id (Info) /= No_Unit_Id);
      return Unit_Id (Info);
   end Unit_Id_Of;

   --------------
   -- Validate --
   --------------

   procedure Validate (Order : Unit_Id_Array; Doing_New : Boolean) is
      Cur_SCC : Unit_Id := No_Unit_Id;
      OK      : Boolean := True;
      Msg     : String := "Old: ";

   begin
      if Doing_New then
         Msg := "New: ";
      end if;

      --  For each unit, assert that its successors are elaborated after it

      for J in Order'Range loop
         declare
            U : constant Unit_Id := Order (J);
            S : Successor_Id := UNR.Table (U).Successors;

         begin
            while S /= No_Successor loop
               if UNR.Table (Succ.Table (S).After).Elab_Position <=
                    UNR.Table (U).Elab_Position
               then
                  OK := False;
                  Write_Line (Msg & " elab order failed");
               end if;

               S := Succ.Table (S).Next;
            end loop;
         end;
      end loop;

      --  An SCC of size 2 units necessarily consists of a spec and the
      --  corresponding body. Assert that the body is elaborated immediately
      --  after the spec, with nothing in between. (We only have SCCs in the
      --  new algorithm.)

      if Doing_New then
         for J in Order'Range loop
            declare
               U : constant Unit_Id := Order (J);

            begin
               if Nodes (U)'Length = 2 then
                  if Units.Table (U).Utype = Is_Spec then
                     if Order (J + 1) /= Corresponding_Body (U) then
                        OK := False;
                        Write_Line (Msg & "Bad spec with SCC of size 2:");
                        Write_SCC (SCC (U));
                     end if;
                  end if;

                  if Units.Table (U).Utype = Is_Body then
                     if Order (J - 1) /= Corresponding_Spec (U) then
                        OK := False;
                        Write_Line (Msg & "Bad body with SCC of size 2:");
                        Write_SCC (SCC (U));
                     end if;
                  end if;
               end if;
            end;
         end loop;

         --  Assert that all units of an SCC are elaborated together, with no
         --  units from other SCCs in between. The above spec/body case is a
         --  special case of this general rule.

         for J in Order'Range loop
            declare
               U : constant Unit_Id := Order (J);

            begin
               if SCC (U) /= Cur_SCC then
                  Cur_SCC := SCC (U);
                  if UNR.Table (Cur_SCC).Validate_Seen then
                     OK := False;
                     Write_Line (Msg & "SCC not elaborated together:");
                     Write_SCC (Cur_SCC);
                  end if;

                  UNR.Table (Cur_SCC).Validate_Seen := True;
               end if;
            end;
         end loop;
      end if;

      pragma Assert (OK);
   end Validate;

   -------------------
   -- Write_Closure --
   -------------------

   procedure Write_Closure (Order : Unit_Id_Array) is
      package Closure_Sources is new Table.Table
        (Table_Component_Type => File_Name_Type,
         Table_Index_Type     => Natural,
         Table_Low_Bound      => 1,
         Table_Initial        => 10,
         Table_Increment      => 100,
         Table_Name           => "Gnatbind.Closure_Sources");
      --  Table to record the sources in the closure, to avoid duplications

      function Put_In_Sources (S : File_Name_Type) return Boolean;
      --  Check if S is already in table Sources and put in Sources if it is
      --  not. Return False if the source is already in Sources, and True if
      --  it is added.

      --------------------
      -- Put_In_Sources --
      --------------------

      function Put_In_Sources (S : File_Name_Type) return Boolean is
      begin
         for J in 1 .. Closure_Sources.Last loop
            if Closure_Sources.Table (J) = S then
               return False;
            end if;
         end loop;

         Closure_Sources.Append (S);
         return True;
      end Put_In_Sources;

      --  Local variables

      Source : File_Name_Type;

   --  Start of processing for Write_Closure

   begin
      Closure_Sources.Init;

      if not Zero_Formatting then
         Write_Eol;
         Write_Line ("REFERENCED SOURCES");
      end if;

      for J in reverse Order'Range loop
         Source := Units.Table (Order (J)).Sfile;

         --  Do not include same source more than once

         if Put_In_Sources (Source)

           --  Do not include run-time units unless -Ra switch set

           and then (List_Closure_All
                      or else not Is_Internal_File_Name (Source))
         then
            if not Zero_Formatting then
               Write_Str ("   ");
            end if;

            Write_Line (Get_Name_String (Source));
         end if;
      end loop;

      --  Subunits do not appear in the elaboration table because they are
      --  subsumed by their parent units, but we need to list them for other
      --  tools. For now they are listed after other files, rather than right
      --  after their parent, since there is no easy link between the
      --  elaboration table and the ALIs table. As subunits may appear
      --  repeatedly in the list, if the parent unit appears in the context of
      --  several units in the closure, duplicates are suppressed.

      for J in Sdep.First .. Sdep.Last loop
         Source := Sdep.Table (J).Sfile;

         if Sdep.Table (J).Subunit_Name /= No_Name
           and then Put_In_Sources (Source)
           and then not Is_Internal_File_Name (Source)
         then
            if not Zero_Formatting then
               Write_Str ("   ");
            end if;

            Write_Line (Get_Name_String (Source));
         end if;
      end loop;

      if not Zero_Formatting then
         Write_Eol;
      end if;
   end Write_Closure;

   ------------------------
   -- Write_Dependencies --
   ------------------------

   procedure Write_Dependencies is
   begin
      if not Zero_Formatting then
         Write_Eol;
         Write_Line ("                 ELABORATION ORDER DEPENDENCIES");
         Write_Eol;
      end if;

      Info_Prefix_Suppress := True;

      for S in Succ_First .. Succ.Last loop
         Elab_Error_Msg (S);
      end loop;

      Info_Prefix_Suppress := False;

      if not Zero_Formatting then
         Write_Eol;
      end if;
   end Write_Dependencies;

   --------------------------
   -- Write_Elab_All_Chain --
   --------------------------

   procedure Write_Elab_All_Chain (S : Successor_Id) is
      ST     : constant Successor_Link := Succ.Table (S);
      After  : constant Unit_Name_Type := Units.Table (ST.After).Uname;

      L   : Elab_All_Id;
      Nam : Unit_Name_Type;

      First_Name : Boolean := True;

   begin
      if ST.Reason in Elab_All .. Elab_All_Desirable then
         L := ST.Elab_All_Link;
         pragma Annotate (CodePeer, Modified, L);

         while L /= No_Elab_All_Link loop
            Nam := Elab_All_Entries.Table (L).Needed_By;
            Error_Msg_Unit_1 := Nam;
            Error_Msg_Output ("        $", Info => True);

            Get_Name_String (Nam);

            if Name_Buffer (Name_Len) = 'b' then
               if First_Name then
                  Error_Msg_Output
                    ("           must be elaborated along with its spec:",
                     Info => True);

               else
                  Error_Msg_Output
                    ("           which must be elaborated along with its "
                     & "spec:",
                     Info => True);
               end if;

            else
               if First_Name then
                  Error_Msg_Output
                    ("           is withed by:",
                     Info => True);

               else
                  Error_Msg_Output
                    ("           which is withed by:",
                     Info => True);
               end if;
            end if;

            First_Name := False;

            L := Elab_All_Entries.Table (L).Next_Elab;
         end loop;

         Error_Msg_Unit_1 := After;
         Error_Msg_Output ("        $", Info => True);
      end if;
   end Write_Elab_All_Chain;

   ----------------------
   -- Write_Elab_Order --
   ----------------------

   procedure Write_Elab_Order
     (Order : Unit_Id_Array; Title : String)
   is
   begin
      if Title /= "" then
         Write_Eol;
         Write_Line (Title);
      end if;

      for J in Order'Range loop
         if not Units.Table (Order (J)).SAL_Interface then
            if not Zero_Formatting then
               Write_Str ("   ");
            end if;

            Write_Unit_Name (Units.Table (Order (J)).Uname);
            Write_Eol;
         end if;
      end loop;

      if Title /= "" then
         Write_Eol;
      end if;
   end Write_Elab_Order;

   --------------
   -- Elab_New --
   --------------

   package body Elab_New is

      generic
         type Node is (<>);
         First_Node : Node;
         Last_Node  : Node;
         type Node_Array is array (Pos range <>) of Node;
         with function Successors (N : Node) return Node_Array;
         with procedure Create_SCC (Root : Node; Nodes : Node_Array);

      procedure Compute_Strongly_Connected_Components;
      --  Compute SCCs for a directed graph. The nodes in the graph are all
      --  values of type Node in the range First_Node .. Last_Node.
      --  Successors(N) returns the nodes pointed to by the edges emanating
      --  from N. Create_SCC is a callback that is called once for each SCC,
      --  passing in the Root node for that SCC (which is an arbitrary node in
      --  the SCC used as a representative of that SCC), and the set of Nodes
      --  in that SCC.
      --
      --  This is generic, in case we want to use it elsewhere; then we could
      --  move this into a separate library unit. Unfortunately, it's not as
      --  generic as one might like. Ideally, we would have "type Node is
      --  private;", and pass in iterators to iterate over all nodes, and over
      --  the successors of a given node. However, that leads to using advanced
      --  features of Ada that are not allowed in the compiler and binder for
      --  bootstrapping reasons. It also leads to trampolines, which are not
      --  allowed in the compiler and binder. Restricting Node to be discrete
      --  allows us to iterate over all nodes with a 'for' loop, and allows us
      --  to attach temporary information to nodes by having an array indexed
      --  by Node.

      procedure Compute_Unit_SCCs;
      --  Use the above generic procedure to compute the SCCs for the graph of
      --  units. Store in each Unit_Node_Record the SCC_Root and Nodes
      --  components. Also initialize the SCC_Num_Pred components.

      procedure Find_Elab_All_Errors;
      --  Generate an error for illegal Elaborate_All pragmas (explicit or
      --  implicit). A pragma Elaborate_All (Y) on unit X is legal if and only
      --  if X and Y are in different SCCs.

      -------------------------------------------
      -- Compute_Strongly_Connected_Components --
      -------------------------------------------

      procedure Compute_Strongly_Connected_Components is

         --  This uses Tarjan's algorithm for finding SCCs. Comments here are
         --  intended to tell what it does, but if you want to know how it
         --  works, you have to look it up. Please do not modify this code
         --  without reading up on Tarjan's algorithm.

         subtype Node_Index is Nat;
         No_Index : constant Node_Index := 0;

         Num_Nodes : constant Nat :=
                       Node'Pos (Last_Node) - Node'Pos (First_Node) + 1;
         Stack : Node_Array (1 .. Num_Nodes);
         Top   : Node_Index := 0;
         --  Stack of nodes, pushed when first visited. All nodes of an SCC are
         --  popped at once when the SCC is found.

         subtype Valid_Node is Node range First_Node .. Last_Node;
         Node_Indices : array (Valid_Node) of Node_Index :=
                          (others => No_Index);
         --  Each node has an "index", which is the sequential number in the
         --  order in which they are visited in the recursive walk. No_Index
         --  means "not yet visited"; we want to avoid walking any node more
         --  than once.

         Index : Node_Index := 1;
         --  Next value to be assigned to a node index

         Low_Links : array (Valid_Node) of Node_Index;
         --  Low_Links (N) is the smallest index of nodes reachable from N

         On_Stack : array (Valid_Node) of Boolean := (others => False);
         --  True if the node is currently on the stack

         procedure Walk (N : Valid_Node);
         --  Recursive depth-first graph walk, with the node index used to
         --  avoid visiting a node more than once.

         ----------
         -- Walk --
         ----------

         procedure Walk (N : Valid_Node) is
            Stack_Position_Of_N : constant Pos := Top + 1;
            S : constant Node_Array := Successors (N);

         begin
            --  Assign the index and low link, increment Index for next call to
            --  Walk.

            Node_Indices (N) := Index;
            Low_Links (N) := Index;
            Index := Index + 1;

            --  Push it on the stack:

            Top := Stack_Position_Of_N;
            Stack (Top) := N;
            On_Stack (N) := True;

            --  Walk not-yet-visited subnodes, and update low link for visited
            --  ones as appropriate.

            for J in S'Range loop
               if Node_Indices (S (J)) = No_Index then
                  Walk (S (J));
                  Low_Links (N) :=
                    Node_Index'Min (Low_Links (N), Low_Links (S (J)));
               elsif On_Stack (S (J)) then
                  Low_Links (N) :=
                    Node_Index'Min (Low_Links (N), Node_Indices (S (J)));
               end if;
            end loop;

            --  If the index is (still) equal to the low link, we've found an
            --  SCC. Pop the whole SCC off the stack, and call Create_SCC.

            if Low_Links (N) = Node_Indices (N) then
               declare
                  SCC : Node_Array renames
                    Stack (Stack_Position_Of_N .. Top);
                  pragma Assert (SCC'Length >= 1);
                  pragma Assert (SCC (SCC'First) = N);

               begin
                  for J in SCC'Range loop
                     On_Stack (SCC (J)) := False;
                  end loop;

                  Create_SCC (Root => N, Nodes => SCC);
                  pragma Assert (Top - SCC'Length = Stack_Position_Of_N - 1);
                  Top := Stack_Position_Of_N - 1; -- pop all
               end;
            end if;
         end Walk;

      --  Start of processing for Compute_Strongly_Connected_Components

      begin
         --  Walk all the nodes that have not yet been walked

         for N in Valid_Node loop
            if Node_Indices (N) = No_Index then
               Walk (N);
            end if;
         end loop;
      end Compute_Strongly_Connected_Components;

      -----------------------
      -- Compute_Unit_SCCs --
      -----------------------

      procedure Compute_Unit_SCCs is
         function Successors (U : Unit_Id) return Unit_Id_Array;
         --  Return all the units that must be elaborated after U. In addition,
         --  if U is a body, include the corresponding spec; this ensures that
         --  a spec/body pair are always in the same SCC.

         procedure Create_SCC (Root : Unit_Id; Nodes : Unit_Id_Array);
         --  Set Nodes of the Root, and set SCC_Root of all the Nodes

         procedure Init_SCC_Num_Pred (U : Unit_Id);
         --  Initialize the SCC_Num_Pred fields, so that the root of each SCC
         --  has a count of the number of successors of all the units in the
         --  SCC, but only for successors outside the SCC.

         procedure Compute_SCCs is new Compute_Strongly_Connected_Components
           (Node       => Unit_Id,
            First_Node => Units.First,
            Last_Node  => Units.Last,
            Node_Array => Unit_Id_Array,
            Successors => Successors,
            Create_SCC => Create_SCC);

         ----------------
         -- Create_SCC --
         ----------------

         procedure Create_SCC (Root : Unit_Id; Nodes : Unit_Id_Array) is
         begin
            if Debug_Flag_V then
               Write_Str ("Root = ");
               Write_Int (Int (Root));
               Write_Str (" ");
               Write_Unit_Name (Units.Table (Root).Uname);
               Write_Str (" -- ");
               Write_Int (Nodes'Length);
               Write_Line (" units:");

               for J in Nodes'Range loop
                  Write_Str ("   ");
                  Write_Int (Int (Nodes (J)));
                  Write_Str (" ");
                  Write_Unit_Name (Units.Table (Nodes (J)).Uname);
                  Write_Eol;
               end loop;
            end if;

            pragma Assert (Nodes (Nodes'First) = Root);
            pragma Assert (UNR.Table (Root).Nodes = null);
            UNR.Table (Root).Nodes := new Unit_Id_Array'(Nodes);

            for J in Nodes'Range loop
               pragma Assert (SCC (Nodes (J)) = No_Unit_Id);
               UNR.Table (Nodes (J)).SCC_Root := Root;
            end loop;
         end Create_SCC;

         ----------------
         -- Successors --
         ----------------

         function Successors (U : Unit_Id) return Unit_Id_Array is
            S   : Successor_Id := UNR.Table (U).Successors;
            Tab : Unit_Id_Table;

         begin
            --  Pretend that a spec is a successor of its body (even though it
            --  isn't), just so both get included.

            if Units.Table (U).Utype = Is_Body then
               Append (Tab, Corresponding_Spec (U));
            end if;

            --  Now include the real successors

            while S /= No_Successor loop
               pragma Assert (Succ.Table (S).Before = U);
               Append (Tab, Succ.Table (S).After);
               S := Succ.Table (S).Next;
            end loop;

            declare
               Result : constant Unit_Id_Array := Tab.Table (1 .. Last (Tab));

            begin
               Free (Tab);
               return Result;
            end;
         end Successors;

         -----------------------
         -- Init_SCC_Num_Pred --
         -----------------------

         procedure Init_SCC_Num_Pred (U : Unit_Id) is
         begin
            if UNR.Table (U).Visited then
               return;
            end if;

            UNR.Table (U).Visited := True;

            declare
               S : Successor_Id := UNR.Table (U).Successors;

            begin
               while S /= No_Successor loop
                  pragma Assert (Succ.Table (S).Before = U);
                  Init_SCC_Num_Pred (Succ.Table (S).After);

                  if SCC (U) /= SCC (Succ.Table (S).After) then
                     UNR.Table (SCC (Succ.Table (S).After)).SCC_Num_Pred :=
                       UNR.Table (SCC (Succ.Table (S).After)).SCC_Num_Pred + 1;
                  end if;

                  S := Succ.Table (S).Next;
               end loop;
            end;
         end Init_SCC_Num_Pred;

      --  Start of processing for Compute_Unit_SCCs

      begin
         Compute_SCCs;

         for Uref in UNR.First .. UNR.Last loop
            pragma Assert (not UNR.Table (Uref).Visited);
            null;
         end loop;

         for Uref in UNR.First .. UNR.Last loop
            Init_SCC_Num_Pred (Uref);
         end loop;

         --  Assert that SCC_Root of all units has been set to a valid unit,
         --  and that SCC_Num_Pred has not been modified in non-root units.

         for Uref in UNR.First .. UNR.Last loop
            pragma Assert (UNR.Table (Uref).SCC_Root /= No_Unit_Id);
            pragma Assert (UNR.Table (Uref).SCC_Root in UNR.First .. UNR.Last);

            if SCC (Uref) /= Uref then
               pragma Assert (UNR.Table (Uref).SCC_Num_Pred = 0);
               null;
            end if;
         end loop;
      end Compute_Unit_SCCs;

      --------------------------
      -- Find_Elab_All_Errors --
      --------------------------

      procedure Find_Elab_All_Errors is
         Withed_Unit : Unit_Id;

      begin
         for U in Units.First .. Units.Last loop

            --  If this unit is not an interface to a stand-alone library,
            --  process WITH references for this unit ignoring interfaces to
            --  stand-alone libraries.

            if not Units.Table (U).SAL_Interface then
               for W in Units.Table (U).First_With ..
                        Units.Table (U).Last_With
               loop
                  if Withs.Table (W).Sfile /= No_File
                    and then (not Withs.Table (W).SAL_Interface)
                  then
                     --  Check for special case of withing a unit that does not
                     --  exist any more.

                     if Get_Name_Table_Int (Withs.Table (W).Uname) = 0 then
                        goto Next_With;
                     end if;

                     Withed_Unit := Unit_Id_Of (Withs.Table (W).Uname);

                     --  If it's Elaborate_All or Elab_All_Desirable, check
                     --  that the withER and withEE are not in the same SCC.

                     if Withs.Table (W).Elaborate_All
                       or else Withs.Table (W).Elab_All_Desirable
                     then
                        if SCC (U) = SCC (Withed_Unit) then
                           Elab_Cycle_Found := True;

                           --  We could probably give better error messages
                           --  than Elab_Old here, but for now, to avoid
                           --  disruption, we don't give any error here.
                           --  Instead, we set the Elab_Cycle_Found flag above,
                           --  and then run the Elab_Old algorithm to issue the
                           --  error message. Ideally, we would like to print
                           --  multiple errors rather than stopping after the
                           --  first cycle.

                           if False then
                              Error_Msg_Output
                                ("illegal pragma Elaborate_All",
                                 Info => False);
                           end if;
                        end if;
                     end if;
                  end if;

                  <<Next_With>>
                  null;
               end loop;
            end if;
         end loop;
      end Find_Elab_All_Errors;

      ---------------------
      -- Find_Elab_Order --
      ---------------------

      procedure Find_Elab_Order (Elab_Order : out Unit_Id_Table) is
         Best_So_Far : Unit_Id;
         U           : Unit_Id;

      begin
         --  Gather dependencies and output them if option set

         Gather_Dependencies;

         Compute_Unit_SCCs;

         --  Initialize the no-predecessor list

         No_Pred := No_Unit_Id;
         for U in UNR.First .. UNR.Last loop
            if UNR.Table (U).Num_Pred = 0 then
               UNR.Table (U).Nextnp := No_Pred;
               No_Pred := U;
            end if;
         end loop;

         --  OK, now we determine the elaboration order proper. All we do is to
         --  select the best choice from the no-predecessor list until all the
         --  nodes have been chosen.

         Outer : loop
            if Debug_Flag_N then
               Write_Line ("Outer loop");
            end if;

            --  If there are no nodes with predecessors, then either we are
            --  done, as indicated by Num_Left being set to zero, or we have a
            --  circularity. In the latter case, diagnose the circularity,
            --  removing it from the graph and
            --  continue. Diagnose_Elaboration_Problem always raises an
            --  exception, so the loop never goes around more than once.

            Get_No_Pred : while No_Pred = No_Unit_Id loop
               exit Outer when Num_Left < 1;
               Diagnose_Elaboration_Problem (Elab_Order);
            end loop Get_No_Pred;

            U := No_Pred;
            Best_So_Far := No_Unit_Id;

            --  Loop to choose best entry in No_Pred list

            No_Pred_Search : loop
               if Debug_Flag_N then
                  Write_Str ("  considering choice of ");
                  Write_Unit_Name (Units.Table (U).Uname);
                  Write_Eol;

                  if Units.Table (U).Elaborate_Body then
                     Write_Str
                       ("    Elaborate_Body = True, Num_Pred for body = ");
                     Write_Int
                       (UNR.Table (Corresponding_Body (U)).Num_Pred);
                  else
                     Write_Str
                       ("    Elaborate_Body = False");
                  end if;

                  Write_Eol;
               end if;

               --  Don't even consider units whose SCC is not ready. This
               --  ensures that all units of an SCC will be elaborated
               --  together, with no other units in between.

               if SCC_Num_Pred (U) = 0
                 and then Better_Choice (U, Best_So_Far)
               then
                  if Debug_Flag_N then
                     Write_Line ("    tentatively chosen (best so far)");
                  end if;

                  Best_So_Far := U;
               else
                  if Debug_Flag_N then
                     Write_Line ("    SCC not ready");
                  end if;
               end if;

               U := UNR.Table (U).Nextnp;
               exit No_Pred_Search when U = No_Unit_Id;
            end loop No_Pred_Search;

            --  If there are no units on the No_Pred list whose SCC is ready,
            --  there must be a cycle. Defer to Elab_Old to print an error
            --  message.

            if Best_So_Far = No_Unit_Id then
               Elab_Cycle_Found := True;
               return;
            end if;

            --  Choose the best candidate found

            Choose (Elab_Order, Best_So_Far, " [Best_So_Far]");

            --  If it's a spec with a body, and the body is not yet chosen,
            --  choose the body if possible. The case where the body is
            --  already chosen is Elaborate_Body; the above call to Choose
            --  the spec will also Choose the body.

            if Units.Table (Best_So_Far).Utype = Is_Spec
              and then UNR.Table
                         (Corresponding_Body (Best_So_Far)).Elab_Position = 0
            then
               declare
                  Choose_The_Body : constant Boolean :=
                                      UNR.Table (Corresponding_Body
                                        (Best_So_Far)).Num_Pred = 0;

               begin
                  if Debug_Flag_B then
                     Write_Str ("Can we choose the body?... ");

                     if Choose_The_Body then
                        Write_Line ("Yes!");
                     else
                        Write_Line ("No.");
                     end if;
                  end if;

                  if Choose_The_Body then
                     Choose
                       (Elab_Order => Elab_Order,
                        Chosen     => Corresponding_Body (Best_So_Far),
                        Msg        => " [body]");
                  end if;
               end;
            end if;

            --  Finally, choose all the rest of the units in the same SCC as
            --  Best_So_Far. If it hasn't been chosen (Elab_Position = 0), and
            --  it's ready to be chosen (Num_Pred = 0), then we can choose it.

            loop
               declare
                  Chose_One_Or_More : Boolean := False;
                  SCC : Unit_Id_Array renames Nodes (Best_So_Far).all;

               begin
                  for J in SCC'Range loop
                     if UNR.Table (SCC (J)).Elab_Position = 0
                       and then UNR.Table (SCC (J)).Num_Pred = 0
                     then
                        Chose_One_Or_More := True;
                        Choose (Elab_Order, SCC (J), " [same SCC]");
                     end if;
                  end loop;

                  exit when not Chose_One_Or_More;
               end;
            end loop;
         end loop Outer;

         Find_Elab_All_Errors;
      end Find_Elab_Order;

      -----------
      -- Nodes --
      -----------

      function Nodes (U : Unit_Id) return Unit_Id_Array_Ptr is
      begin
         return UNR.Table (SCC (U)).Nodes;
      end Nodes;

      ---------
      -- SCC --
      ---------

      function SCC (U : Unit_Id) return Unit_Id is
      begin
         return UNR.Table (U).SCC_Root;
      end SCC;

      ------------------
      -- SCC_Num_Pred --
      ------------------

      function SCC_Num_Pred (U : Unit_Id) return Int is
      begin
         return UNR.Table (SCC (U)).SCC_Num_Pred;
      end SCC_Num_Pred;

      ---------------
      -- Write_SCC --
      ---------------

      procedure Write_SCC (U : Unit_Id) is
         pragma Assert (SCC (U) = U);
      begin
         for J in Nodes (U)'Range loop
            Write_Int (UNR.Table (Nodes (U) (J)).Elab_Position);
            Write_Str (". ");
            Write_Unit_Name (Units.Table (Nodes (U) (J)).Uname);
            Write_Eol;
         end loop;

         Write_Eol;
      end Write_SCC;

   end Elab_New;

   --------------
   -- Elab_Old --
   --------------

   package body Elab_Old is

      ---------------------
      -- Find_Elab_Order --
      ---------------------

      procedure Find_Elab_Order (Elab_Order : out Unit_Id_Table) is
         Best_So_Far : Unit_Id;
         U           : Unit_Id;

      begin
         --  Gather dependencies and output them if option set

         Gather_Dependencies;

         --  Initialize the no-predecessor list

         No_Pred := No_Unit_Id;
         for U in UNR.First .. UNR.Last loop
            if UNR.Table (U).Num_Pred = 0 then
               UNR.Table (U).Nextnp := No_Pred;
               No_Pred := U;
            end if;
         end loop;

         --  OK, now we determine the elaboration order proper. All we do is to
         --  select the best choice from the no-predecessor list until all the
         --  nodes have been chosen.

         Outer : loop

            --  If there are no nodes with predecessors, then either we are
            --  done, as indicated by Num_Left being set to zero, or we have a
            --  circularity. In the latter case, diagnose the circularity,
            --  removing it from the graph and continue.
            --  Diagnose_Elaboration_Problem always raises an exception, so the
            --  loop never goes around more than once.

            Get_No_Pred : while No_Pred = No_Unit_Id loop
               exit Outer when Num_Left < 1;
               Diagnose_Elaboration_Problem (Elab_Order);
            end loop Get_No_Pred;

            U := No_Pred;
            Best_So_Far := No_Unit_Id;

            --  Loop to choose best entry in No_Pred list

            No_Pred_Search : loop
               if Debug_Flag_N then
                  Write_Str ("  considering choice of ");
                  Write_Unit_Name (Units.Table (U).Uname);
                  Write_Eol;

                  if Units.Table (U).Elaborate_Body then
                     Write_Str
                       ("    Elaborate_Body = True, Num_Pred for body = ");
                     Write_Int
                       (UNR.Table (Corresponding_Body (U)).Num_Pred);
                  else
                     Write_Str
                       ("    Elaborate_Body = False");
                  end if;

                  Write_Eol;
               end if;

               --  This is a candididate to be considered for choice

               if Better_Choice (U, Best_So_Far) then
                  if Debug_Flag_N then
                     Write_Line ("    tentatively chosen (best so far)");
                  end if;

                  Best_So_Far := U;
               end if;

               U := UNR.Table (U).Nextnp;
               exit No_Pred_Search when U = No_Unit_Id;
            end loop No_Pred_Search;

            --  Choose the best candidate found

            Choose (Elab_Order, Best_So_Far, " [Elab_Old Best_So_Far]");
         end loop Outer;
      end Find_Elab_Order;

   end Elab_Old;

end Binde;

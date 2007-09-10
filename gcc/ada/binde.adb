------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                B I N D E                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2007, Free Software Foundation, Inc.         --
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

with Binderr;  use Binderr;
with Butil;    use Butil;
with Debug;    use Debug;
with Fname;    use Fname;
with Namet;    use Namet;
with Opt;      use Opt;
with Osint;
with Output;   use Output;
with Targparm; use Targparm;

with System.Case_Util; use System.Case_Util;

package body Binde is

   --  The following data structures are used to represent the graph that is
   --  used to determine the elaboration order (using a topological sort).

   --  The following structures are used to record successors. If A is a
   --  successor of B in this table, it means that A must be elaborated
   --  before B is elaborated.

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

      Elab,
      --  After directly mentions Before in a pragma Elaborate, so the
      --  body of Before must be elaborate before After is elaborated.

      Elab_All,
      --  After either mentions Before directly in a pragma Elaborate_All,
      --  or mentions a third unit, X, which itself requires that Before be
      --  elaborated before unit X is elaborated. The Elab_All_Link list
      --  traces the dependencies in the latter case.

      Elab_All_Desirable,
      --  This is just like Elab_All, except that the elaborate all was not
      --  explicitly present in the source, but rather was created by the
      --  front end, which decided that it was "desirable".

      Elab_Desirable,
      --  This is just like Elab, except that the elaborate was not
      --  explicitly present in the source, but rather was created by the
      --  front end, which decided that it was "desirable".

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
      --  first elment in a list of Elab_All entries that record the with
      --  chain leading resulting in this particular dependency.

   end record;

   --  Note on handling of Elaborate_Body. Basically, if we have a pragma
   --  Elaborate_Body in a unit, it means that the spec and body have to
   --  be handled as a single entity from the point of view of determining
   --  an elaboration order. What we do is to essentially remove the body
   --  from consideration completely, and transfer all its links (other
   --  than the spec link) to the spec. Then when then the spec gets chosen,
   --  we choose the body right afterwards. We mark the links that get moved
   --  from the body to the spec by setting their Elab_Body flag True, so
   --  that we can understand what is going on!

   Succ_First : constant := 1;

   package Succ is new Table.Table (
     Table_Component_Type => Successor_Link,
     Table_Index_Type     => Successor_Id,
     Table_Low_Bound      => Succ_First,
     Table_Initial        => 500,
     Table_Increment      => 200,
     Table_Name           => "Succ");

   --  For the case of Elaborate_All, the following table is used to record
   --  chains of with relationships that lead to the Elab_All link. These
   --  are used solely for diagnostic purposes

   type Elab_All_Entry is record
      Needed_By : Unit_Name_Type;
      --  Name of unit from which referencing unit was with'ed or otherwise
      --  needed as a result of Elaborate_All or Elaborate_Desirable.

      Next_Elab : Elab_All_Id;
      --  Link to next entry on chain (No_Elab_All_Link marks end of list)
   end record;

   package Elab_All_Entries is new Table.Table (
     Table_Component_Type => Elab_All_Entry,
     Table_Index_Type     => Elab_All_Id,
     Table_Low_Bound      => 1,
     Table_Initial        => 2000,
     Table_Increment      => 200,
     Table_Name           => "Elab_All_Entries");

   --  A Unit_Node record is built for each active unit

   type Unit_Node_Record is record

      Successors : Successor_Id;
      --  Pointer to list of links for successor nodes

      Num_Pred : Int;
      --  Number of predecessors for this unit. Normally non-negative, but
      --  can go negative in the case of units chosen by the diagnose error
      --  procedure (when cycles are being removed from the graph).

      Nextnp : Unit_Id;
      --  Forward pointer for list of units with no predecessors

      Elab_Order : Nat;
      --  Position in elaboration order (zero = not placed yet)

      Visited : Boolean;
      --  Used in computing transitive closure for elaborate all and
      --  also in locating cycles and paths in the diagnose routines.

      Elab_Position : Natural;
      --  Initialized to zero. Set non-zero when a unit is chosen and
      --  placed in the elaboration order. The value represents the
      --  ordinal position in the elaboration order.

   end record;

   package UNR is new Table.Table (
     Table_Component_Type => Unit_Node_Record,
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
   --  Current unit, set by Gather_Dependencies, and picked up in Build_Link
   --  to set the Reason_Unit field of the created dependency link.

   Num_Chosen : Natural := 0;
   --  Number of units chosen in the elaboration order so far

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Better_Choice (U1, U2 : Unit_Id) return Boolean;
   --  U1 and U2 are both permitted candidates for selection as the next unit
   --  to be elaborated. This function determines whether U1 is a better choice
   --  than U2, i.e. should be elaborated in preference to U2, based on a set
   --  of heuristics that establish a friendly and predictable order (see body
   --  for details). The result is True if U1 is a better choice than U2, and
   --  False if it is a worse choice, or there is no preference between them.

   procedure Build_Link
     (Before : Unit_Id;
      After  : Unit_Id;
      R      : Succ_Reason;
      Ea_Id  : Elab_All_Id := No_Elab_All_Link);
   --  Establish a successor link, Before must be elaborated before After,
   --  and the reason for the link is R. Ea_Id is the contents to be placed
   --  in the Elab_All_Link of the entry.

   procedure Choose (Chosen : Unit_Id);
   --  Chosen is the next entry chosen in the elaboration order. This
   --  procedure updates all data structures appropriately.

   function Corresponding_Body (U : Unit_Id) return Unit_Id;
   pragma Inline (Corresponding_Body);
   --  Given a unit which is a spec for which there is a separate body,
   --  return the unit id of the body. It is an error to call this routine
   --  with a unit that is not a spec, or which does not have a separate body.

   function Corresponding_Spec (U : Unit_Id) return Unit_Id;
   pragma Inline (Corresponding_Spec);
   --  Given a unit which is a body for which there is a separate spec,
   --  return the unit id of the spec. It is an error to call this routine
   --  with a unit that is not a body, or which does not have a separate spec.

   procedure Diagnose_Elaboration_Problem;
   --  Called when no elaboration order can be found. Outputs an appropriate
   --  diagnosis of the problem, and then abandons the bind.

   procedure Elab_All_Links
     (Before : Unit_Id;
      After  : Unit_Id;
      Reason : Succ_Reason;
      Link   : Elab_All_Id);
   --  Used to compute the transitive closure of elaboration links for an
   --  Elaborate_All pragma (Reason = Elab_All) or for an indication of
   --  Elaborate_All_Desirable (Reason = Elab_All_Desirable). Unit After has
   --  a pragma Elaborate_All or the front end has determined that a reference
   --  probably requires Elaborate_All is required, and unit Before must be
   --  previously elaborated. First a link is built making sure that unit
   --  Before is elaborated before After, then a recursive call ensures that
   --  we also build links for any units needed by Before (i.e. these units
   --  must/should also be elaborated before After). Link is used to build
   --  a chain of Elab_All_Entries to explain the reason for a link. The
   --  value passed is the chain so far.

   procedure Elab_Error_Msg (S : Successor_Id);
   --  Given a successor link, outputs an error message of the form
   --  "$ must be elaborated before $ ..." where ... is the reason.

   procedure Gather_Dependencies;
   --  Compute dependencies, building the Succ and UNR tables

   function Is_Body_Unit (U : Unit_Id) return Boolean;
   pragma Inline (Is_Body_Unit);
   --  Determines if given unit is a body

   function Is_Waiting_Body (U : Unit_Id) return Boolean;
   pragma Inline (Is_Waiting_Body);
   --  Determines if U is a waiting body, defined as a body which has
   --  not been elaborated, but whose spec has been elaborated.

   function Make_Elab_Entry
     (Unam : Unit_Name_Type;
      Link : Elab_All_Id) return Elab_All_Id;
   --  Make an Elab_All_Entries table entry with the given Unam and Link

   function Unit_Id_Of (Uname : Unit_Name_Type) return Unit_Id;
   --  This function uses the Info field set in the names table to obtain
   --  the unit Id of a unit, given its name id value.

   function Worse_Choice (U1, U2 : Unit_Id) return Boolean;
   --  This is like Better_Choice, and has the same interface, but returns
   --  true if U1 is a worse choice than U2 in the sense of the -h (horrible
   --  elaboration order) switch. We still have to obey Ada rules, so it is
   --  not quite the direct inverse of Better_Choice.

   procedure Write_Dependencies;
   --  Write out dependencies (called only if appropriate option is set)

   procedure Write_Elab_All_Chain (S : Successor_Id);
   --  If the reason for the link S is Elaborate_All or Elaborate_Desirable,
   --  then this routine will output the "needed by" explanation chain.

   -------------------
   -- Better_Choice --
   -------------------

   function Better_Choice (U1, U2 : Unit_Id) return Boolean is
      UT1 : Unit_Record renames Units.Table (U1);
      UT2 : Unit_Record renames Units.Table (U2);

   begin
      if Debug_Flag_B then
         Write_Str ("Better_Choice (");
         Write_Unit_Name (UT1.Uname);
         Write_Str (", ");
         Write_Unit_Name (UT2.Uname);
         Write_Line (")");
      end if;

      --  Note: the checks here are applied in sequence, and the ordering is
      --  significant (i.e. the more important criteria are applied first).

      --  Prefer a waiting body to any other case

      if Is_Waiting_Body (U1) and not Is_Waiting_Body (U2) then
         if Debug_Flag_B then
            Write_Line ("  True: u1 is waiting body, u2 is not");
         end if;

         return True;

      elsif Is_Waiting_Body (U2) and not Is_Waiting_Body (U1) then
         if Debug_Flag_B then
            Write_Line ("  False: u2 is waiting body, u1 is not");
         end if;

         return False;

      --  Prefer a predefined unit to a non-predefined unit

      elsif UT1.Predefined and not UT2.Predefined then
         if Debug_Flag_B then
            Write_Line ("  True: u1 is predefined, u2 is not");
         end if;

         return True;

      elsif UT2.Predefined and not UT1.Predefined then
         if Debug_Flag_B then
            Write_Line ("  False: u2 is predefined, u1 is not");
         end if;

         return False;

      --  Prefer an internal unit to a non-internal unit

      elsif UT1.Internal and not UT2.Internal then
         if Debug_Flag_B then
            Write_Line ("  True: u1 is internal, u2 is not");
         end if;
         return True;

      elsif UT2.Internal and not UT1.Internal then
         if Debug_Flag_B then
            Write_Line ("  False: u2 is internal, u1 is not");
         end if;

         return False;

      --  Prefer a body to a spec

      elsif Is_Body_Unit (U1) and not Is_Body_Unit (U2) then
         if Debug_Flag_B then
            Write_Line ("  True: u1 is body, u2 is not");
         end if;

         return True;

      elsif Is_Body_Unit (U2) and not Is_Body_Unit (U1) then
         if Debug_Flag_B then
            Write_Line ("  False: u2 is body, u1 is not");
         end if;

         return False;

      --  If both are waiting bodies, then prefer the one whose spec is
      --  more recently elaborated. Consider the following:

      --     spec of A
      --     spec of B
      --     body of A or B?

      --  The normal waiting body preference would have placed the body of
      --  A before the spec of B if it could. Since it could not, there it
      --  must be the case that A depends on B. It is therefore a good idea
      --  to put the body of B first.

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

      if not Debug_Flag_O then

         --  The following deal with the case of specs which have been marked
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

      --  If we fall through, it means that no preference rule applies, so we
      --  use alphabetical order to at least give a deterministic result.

      if Debug_Flag_B then
         Write_Line ("  choose on alpha order");
      end if;

      return Uname_Less (UT1.Uname, UT2.Uname);
   end Better_Choice;

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
      Succ.Increment_Last;
      Succ.Table (Succ.Last).Before          := Before;
      Succ.Table (Succ.Last).Next            := UNR.Table (Before).Successors;
      UNR.Table (Before).Successors          := Succ.Last;
      Succ.Table (Succ.Last).Reason          := R;
      Succ.Table (Succ.Last).Reason_Unit     := Cur_Unit;
      Succ.Table (Succ.Last).Elab_All_Link   := Ea_Id;

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

      Succ.Table (Succ.Last).After           := After;
      Succ.Table (Succ.Last).Elab_Body       := False;
      UNR.Table (After).Num_Pred             := UNR.Table (After).Num_Pred + 1;
   end Build_Link;

   ------------
   -- Choose --
   ------------

   procedure Choose (Chosen : Unit_Id) is
      S : Successor_Id;
      U : Unit_Id;

   begin
      if Debug_Flag_C then
         Write_Str ("Choosing Unit ");
         Write_Unit_Name (Units.Table (Chosen).Uname);
         Write_Eol;
      end if;

      --  Add to elaboration order. Note that units having no elaboration
      --  code are not treated specially yet. The special casing of this
      --  is in Bindgen, where Gen_Elab_Calls skips over them. Meanwhile
      --  we need them here, because the object file list is also driven
      --  by the contents of the Elab_Order table.

      Elab_Order.Increment_Last;
      Elab_Order.Table (Elab_Order.Last) := Chosen;

      --  Remove from No_Pred list. This is a little inefficient and may
      --  be we should doubly link the list, but it will do for now!

      if No_Pred = Chosen then
         No_Pred := UNR.Table (Chosen).Nextnp;

      else
         --  Note that we just ignore the situation where it does not
         --  appear in the No_Pred list, this happens in calls from the
         --  Diagnose_Elaboration_Problem routine, where cycles are being
         --  removed arbitrarily from the graph.

         U := No_Pred;
         while U /= No_Unit_Id loop
            if UNR.Table (U).Nextnp = Chosen then
               UNR.Table (U).Nextnp := UNR.Table (Chosen).Nextnp;
               exit;
            end if;

            U := UNR.Table (U).Nextnp;
         end loop;
      end if;

      --  For all successors, decrement the number of predecessors, and
      --  if it becomes zero, then add to no predecessor list.

      S := UNR.Table (Chosen).Successors;
      while S /= No_Successor loop
         U := Succ.Table (S).After;
         UNR.Table (U).Num_Pred := UNR.Table (U).Num_Pred - 1;

         if Debug_Flag_N then
            Write_Str ("  decrementing Num_Pred for unit ");
            Write_Unit_Name (Units.Table (U).Uname);
            Write_Str (" new value = ");
            Write_Int (Int (UNR.Table (U).Num_Pred));
            Write_Eol;
         end if;

         if UNR.Table (U).Num_Pred = 0 then
            UNR.Table (U).Nextnp := No_Pred;
            No_Pred := U;
         end if;

         S := Succ.Table (S).Next;
      end loop;

      --  All done, adjust number of units left count and set elaboration pos

      Num_Left := Num_Left - 1;
      Num_Chosen := Num_Chosen + 1;
      UNR.Table (Chosen).Elab_Position := Num_Chosen;
      Units.Table (Chosen).Elab_Position := Num_Chosen;

      --  If we just chose a spec with Elaborate_Body set, then we
      --  must immediately elaborate the body, before any other units.

      if Units.Table (Chosen).Elaborate_Body then

         --  If the unit is a spec only, then there is no body. This is a bit
         --  odd given that Elaborate_Body is here, but it is valid in an
         --  RCI unit, where we only have the interface in the stub bind.

         if Units.Table (Chosen).Utype = Is_Spec_Only
           and then Units.Table (Chosen).RCI
         then
            null;
         else
            Choose (Corresponding_Body (Chosen));
         end if;
      end if;
   end Choose;

   ------------------------
   -- Corresponding_Body --
   ------------------------

   --  Currently if the body and spec are separate, then they appear as
   --  two separate units in the same ALI file, with the body appearing
   --  first and the spec appearing second.

   function Corresponding_Body (U : Unit_Id) return Unit_Id is
   begin
      pragma Assert (Units.Table (U).Utype = Is_Spec);
      return U - 1;
   end Corresponding_Body;

   ------------------------
   -- Corresponding_Spec --
   ------------------------

   --  Currently if the body and spec are separate, then they appear as
   --  two separate units in the same ALI file, with the body appearing
   --  first and the spec appearing second.

   function Corresponding_Spec (U : Unit_Id) return Unit_Id is
   begin
      pragma Assert (Units.Table (U).Utype = Is_Body);
      return U + 1;
   end Corresponding_Spec;

   ----------------------------------
   -- Diagnose_Elaboration_Problem --
   ----------------------------------

   procedure Diagnose_Elaboration_Problem is

      function Find_Path (Ufrom, Uto : Unit_Id; ML : Nat) return Boolean;
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

      function Find_Path (Ufrom, Uto : Unit_Id; ML : Nat) return Boolean is

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
            --  Recursion ends if we are at terminating node and the path
            --  is sufficiently long, generate error message and return True.

            if U = Uto and then PL >= ML then
               Choose (U);
               return True;

            --  All done if already visited, otherwise mark as visited

            elsif UNR.Table (U).Visited then
               return False;

            --  Otherwise mark as visited and look at all successors

            else
               UNR.Table (U).Visited := True;

               S := UNR.Table (U).Successors;
               while S /= No_Successor loop
                  if Find_Link (Succ.Table (S).After, PL + 1) then
                     Elab_Error_Msg (S);
                     Choose (U);
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
         --  Initialize all non-chosen nodes to not visisted yet

         for U in Units.First .. Units.Last loop
            UNR.Table (U).Visited := UNR.Table (U).Elab_Position /= 0;
         end loop;

         --  Now try to find the path

         return Find_Link (Ufrom, 0);
      end Find_Path;

   --  Start of processing for Diagnose_Elaboration_Error

   begin
      Set_Standard_Error;

      --  Output state of things if debug flag N set

      if Debug_Flag_N then
         declare
            NP : Int;

         begin
            Write_Eol;
            Write_Eol;
            Write_Str ("Diagnose_Elaboration_Problem called");
            Write_Eol;
            Write_Str ("List of remaining unchosen units and predecessors");
            Write_Eol;

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
                  Write_Char (')');
                  Write_Eol;

                  if NP = 0 then
                     if Units.Table (U).Elaborate_Body then
                        Write_Str
                          ("    (not chosen because of Elaborate_Body)");
                        Write_Eol;
                     else
                        Write_Str ("  ****************** why not chosen?");
                        Write_Eol;
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
                     Write_Str ("  **************** Num_Pred value wrong!");
                     Write_Eol;
                  end if;
               end if;
            end loop;
         end;
      end if;

      --  Output the header for the error, and manually increment the
      --  error count. We are using Error_Msg_Output rather than Error_Msg
      --  here for two reasons:

      --    This is really only one error, not one for each line
      --    We want this output on standard output since it is voluminous

      --  But we do need to deal with the error count manually in this case

      Errors_Detected := Errors_Detected + 1;
      Error_Msg_Output ("elaboration circularity detected", Info => False);

      --  Try to find cycles starting with any of the remaining nodes that have
      --  not yet been chosen. There must be at least one (there is some reason
      --  we are being called!)

      for U in Units.First .. Units.Last loop
         if UNR.Table (U).Elab_Position = 0 then
            if Find_Path (U, U, 1) then
               raise Unrecoverable_Error;
            end if;
         end if;
      end loop;

      --  We should never get here, since we were called for some reason,
      --  and we should have found and eliminated at least one bad path.

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

      for W in
        Units.Table (Before).First_With .. Units.Table (Before).Last_With
      loop
         --  Skip if this with is an interface to a stand-alone library.
         --  Skip also if no ALI file for this WITH, happens for language
         --  defined generics while bootstrapping the compiler (see body of
         --  Lib.Writ.Write_With_Lines).

         if not Withs.Table (W).SAL_Interface
           and then Withs.Table (W).Afile /= No_File
         then
            declare
               Info : constant Int :=
                        Get_Name_Table_Info
                          (Withs.Table (W).Uname);

            begin
               --  If the unit is unknown, for some unknown reason, fail
               --  graciously explaining that the unit is unknown. Without
               --  this check, gnatbind will crash in Unit_Id_Of.

               if Info = 0 or else Unit_Id (Info) = No_Unit_Id then
                  declare
                     Withed       : String :=
                                      Get_Name_String (Withs.Table (W).Uname);
                     Last_Withed  : Natural := Withed'Last;
                     Withing      : String :=
                                      Get_Name_String
                                        (Units.Table (Before).Uname);
                     Last_Withing : Natural := Withing'Last;
                     Spec_Body    : String  := " (Spec)";

                  begin
                     To_Mixed (Withed);
                     To_Mixed (Withing);

                     if Last_Withed > 2 and then
                       Withed (Last_Withed - 1) = '%'
                     then
                        Last_Withed := Last_Withed - 2;
                     end if;

                     if Last_Withing > 2 and then
                       Withing (Last_Withing - 1) = '%'
                     then
                        Last_Withing := Last_Withing - 2;
                     end if;

                     if Units.Table (Before).Utype = Is_Body or else
                       Units.Table (Before).Utype = Is_Body_Only
                     then
                        Spec_Body := " (Body)";
                     end if;

                     Osint.Fail
                       ("could not find unit ",
                        Withed (Withed'First .. Last_Withed) & " needed by " &
                        Withing (Withing'First .. Last_Withing) & Spec_Body);
                  end;
               end if;

               Elab_All_Links
                 (Unit_Id_Of (Withs.Table (W).Uname),
                  After,
                  Reason,
                  Make_Elab_Entry (Withs.Table (W).Uname, Link));
            end;
         end if;
      end loop;

      --  Process corresponding body, if there is one

      if Units.Table (Before).Utype = Is_Spec then
         Elab_All_Links
           (Corresponding_Body (Before),
            After, Reason,
            Make_Elab_Entry
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
              ("     recompile $ with -gnatwl for full details",
               Info => True);

         when Elab_Desirable =>
            Error_Msg_Output
              ("     reason: implicit Elaborate in unit $",
               Info => True);

            Error_Msg_Output
              ("     recompile $ with -gnatwl for full details",
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
           ("  $ must therefore be elaborated before $",
            True);

         Error_Msg_Unit_1 := Units.Table (SL.After).Uname;
         Error_Msg_Output
           ("     (because $ has a pragma Elaborate_Body)",
            True);
      end if;

      if not Zero_Formatting then
         Write_Eol;
      end if;
   end Elab_Error_Msg;

   ---------------------
   -- Find_Elab_Order --
   ---------------------

   procedure Find_Elab_Order is
      U           : Unit_Id;
      Best_So_Far : Unit_Id;

   begin
      Succ.Init;
      Num_Left := Int (Units.Last - Units.First + 1);

      --  Initialize unit table for elaboration control

      for U in Units.First .. Units.Last loop
         UNR.Increment_Last;
         UNR.Table (UNR.Last).Successors    := No_Successor;
         UNR.Table (UNR.Last).Num_Pred      := 0;
         UNR.Table (UNR.Last).Nextnp        := No_Unit_Id;
         UNR.Table (UNR.Last).Elab_Order    := 0;
         UNR.Table (UNR.Last).Elab_Position := 0;
      end loop;

      --  Output warning if -p used with no -gnatE units

      if Pessimistic_Elab_Order
        and not Dynamic_Elaboration_Checks_Specified
      then
         if OpenVMS_On_Target then
            Error_Msg ("?use of /PESSIMISTIC_ELABORATION questionable");
         else
            Error_Msg ("?use of -p switch questionable");
         end if;

         Error_Msg ("?since all units compiled with static elaboration model");
      end if;

      --  Gather dependencies and output them if option set

      Gather_Dependencies;

      --  Output elaboration dependencies if option is set

      if Elab_Dependency_Output or Debug_Flag_E then
         Write_Dependencies;
      end if;

      --  Initialize the no predecessor list

      No_Pred := No_Unit_Id;

      for U in UNR.First .. UNR.Last loop
         if UNR.Table (U).Num_Pred = 0 then
            UNR.Table (U).Nextnp := No_Pred;
            No_Pred := U;
         end if;
      end loop;

      --  OK, now we determine the elaboration order proper. All we do is to
      --  select the best choice from the no predecessor list until all the
      --  nodes have been chosen.

      Outer : loop

         --  If there are no nodes with predecessors, then either we are
         --  done, as indicated by Num_Left being set to zero, or we have
         --  a circularity. In the latter case, diagnose the circularity,
         --  removing it from the graph and continue

         Get_No_Pred : while No_Pred = No_Unit_Id loop
            exit Outer when Num_Left < 1;
            Diagnose_Elaboration_Problem;
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
                    (Int (UNR.Table (Corresponding_Body (U)).Num_Pred));
               else
                  Write_Str
                    ("    Elaborate_Body = False");
               end if;

               Write_Eol;
            end if;

            --  This is a candididate to be considered for choice

            if Best_So_Far = No_Unit_Id
              or else ((not Pessimistic_Elab_Order)
                         and then Better_Choice (U, Best_So_Far))
              or else (Pessimistic_Elab_Order
                         and then Worse_Choice (U, Best_So_Far))
            then
               if Debug_Flag_N then
                  Write_Str ("    tentatively chosen (best so far)");
                  Write_Eol;
               end if;

               Best_So_Far := U;
            end if;

            U := UNR.Table (U).Nextnp;
            exit No_Pred_Search when U = No_Unit_Id;
         end loop No_Pred_Search;

         --  If no candididate chosen, it means that no unit has No_Pred = 0,
         --  but there are units left, hence we have a circular dependency,
         --  which we will get Diagnose_Elaboration_Problem to diagnose it.

         if Best_So_Far = No_Unit_Id then
            Diagnose_Elaboration_Problem;

         --  Otherwise choose the best candidate found

         else
            Choose (Best_So_Far);
         end if;
      end loop Outer;
   end Find_Elab_Order;

   -------------------------
   -- Gather_Dependencies --
   -------------------------

   procedure Gather_Dependencies is
      Withed_Unit : Unit_Id;

   begin
      --  Loop through all units

      for U in Units.First .. Units.Last loop
         Cur_Unit := U;

         --  If this is not an interface to a stand-alone library and
         --  there is a body and a spec, then spec must be elaborated first
         --  Note that the corresponding spec immediately follows the body

         if not Units.Table (U).SAL_Interface
           and then Units.Table (U).Utype = Is_Body
         then
            Build_Link (Corresponding_Spec (U), U, Spec_First);
         end if;

         --  If this unit is not an interface to a stand-alone library,
         --  process WITH references for this unit ignoring generic units and
         --  interfaces to stand-alone libraries.

         if not Units.Table (U).SAL_Interface then
            for
              W in Units.Table (U).First_With .. Units.Table (U).Last_With
            loop
               if Withs.Table (W).Sfile /= No_File
                 and then (not Withs.Table (W).SAL_Interface)
               then
                  --  Check for special case of withing a unit that does not
                  --  exist any more. If the unit was completely missing we
                  --  would already have detected this, but a nasty case arises
                  --  when we have a subprogram body with no spec, and some
                  --  obsolete unit with's a previous (now disappeared) spec.

                  if Get_Name_Table_Info (Withs.Table (W).Uname) = 0 then
                     Error_Msg_File_1 := Units.Table (U).Sfile;
                     Error_Msg_Unit_1 := Withs.Table (W).Uname;
                     Error_Msg ("{ depends on $ which no longer exists");
                     goto Next_With;
                  end if;

                  Withed_Unit :=
                    Unit_Id (Unit_Id_Of (Withs.Table (W).Uname));

                  --  Pragma Elaborate_All case, for this we use the recursive
                  --  Elab_All_Links procedure to establish the links.

                  if Withs.Table (W).Elaborate_All then

                     --  Reset flags used to stop multiple visits to a given
                     --  node.

                     for Uref in UNR.First .. UNR.Last loop
                        UNR.Table (Uref).Visited := False;
                     end loop;

                     --  Now establish all the links we need

                     Elab_All_Links
                       (Withed_Unit, U, Elab_All,
                        Make_Elab_Entry
                          (Withs.Table (W).Uname, No_Elab_All_Link));

                     --  Elaborate_All_Desirable case, for this we establish
                     --  the same links as above, but with a different reason.

                  elsif Withs.Table (W).Elab_All_Desirable then

                     --  Reset flags used to stop multiple visits to a given
                     --  node.

                     for Uref in UNR.First .. UNR.Last loop
                        UNR.Table (Uref).Visited := False;
                     end loop;

                     --  Now establish all the links we need

                     Elab_All_Links
                       (Withed_Unit, U, Elab_All_Desirable,
                        Make_Elab_Entry
                          (Withs.Table (W).Uname, No_Elab_All_Link));

                     --  Pragma Elaborate case. We must build a link for the
                     --  withed unit itself, and also the corresponding body
                     --  if there is one.

                     --  However, skip this processing if there is no ALI file
                     --  for the WITH entry, because this means it is a
                     --  generic (even when we fix the generics so that an ALI
                     --  file is present, we probably still will have no ALI
                     --  file for unchecked and other special cases).

                  elsif Withs.Table (W).Elaborate
                    and then Withs.Table (W).Afile /= No_File
                  then
                     Build_Link (Withed_Unit, U, Withed);

                     if Units.Table (Withed_Unit).Utype = Is_Spec then
                        Build_Link
                          (Corresponding_Body (Withed_Unit), U, Elab);
                     end if;

                     --  Elaborate_Desirable case, for this we establish
                     --  the same links as above, but with a different reason.

                  elsif Withs.Table (W).Elab_Desirable then
                     Build_Link (Withed_Unit, U, Withed);

                     if Units.Table (Withed_Unit).Utype = Is_Spec then
                        Build_Link
                          (Corresponding_Body (Withed_Unit),
                           U, Elab_Desirable);
                     end if;

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
   end Gather_Dependencies;

   ------------------
   -- Is_Body_Unit --
   ------------------

   function Is_Body_Unit (U : Unit_Id) return Boolean is
   begin
      return Units.Table (U).Utype = Is_Body
        or else Units.Table (U).Utype = Is_Body_Only;
   end Is_Body_Unit;

   ---------------------
   -- Is_Waiting_Body --
   ---------------------

   function Is_Waiting_Body (U : Unit_Id) return Boolean is
   begin
      return Units.Table (U).Utype = Is_Body
        and then UNR.Table (Corresponding_Spec (U)).Elab_Position /= 0;
   end Is_Waiting_Body;

   ---------------------
   -- Make_Elab_Entry --
   ---------------------

   function Make_Elab_Entry
     (Unam : Unit_Name_Type;
      Link : Elab_All_Id) return Elab_All_Id
   is
   begin
      Elab_All_Entries.Increment_Last;
      Elab_All_Entries.Table (Elab_All_Entries.Last).Needed_By := Unam;
      Elab_All_Entries.Table (Elab_All_Entries.Last).Next_Elab := Link;
      return Elab_All_Entries.Last;
   end Make_Elab_Entry;

   ----------------
   -- Unit_Id_Of --
   ----------------

   function Unit_Id_Of (Uname : Unit_Name_Type) return Unit_Id is
      Info : constant Int := Get_Name_Table_Info (Uname);
   begin
      pragma Assert (Info /= 0 and then Unit_Id (Info) /= No_Unit_Id);
      return Unit_Id (Info);
   end Unit_Id_Of;

   ------------------
   -- Worse_Choice --
   ------------------

   function Worse_Choice (U1, U2 : Unit_Id) return Boolean is
      UT1 : Unit_Record renames Units.Table (U1);
      UT2 : Unit_Record renames Units.Table (U2);

   begin
      --  Note: the checks here are applied in sequence, and the ordering is
      --  significant (i.e. the more important criteria are applied first).

      --  If either unit is internal, then use Better_Choice, since the
      --  language requires that predefined units not mess up in the choice
      --  of elaboration order, and for internal units, any problems are
      --  ours and not the programmers.

      if UT1.Internal or else UT2.Internal then
         return Better_Choice (U1, U2);

      --  Prefer anything else to a waiting body (!)

      elsif Is_Waiting_Body (U1) and not Is_Waiting_Body (U2) then
         return False;

      elsif Is_Waiting_Body (U2) and not Is_Waiting_Body (U1) then
         return True;

      --  Prefer a spec to a body (!)

      elsif Is_Body_Unit (U1) and not Is_Body_Unit (U2) then
         return False;

      elsif Is_Body_Unit (U2) and not Is_Body_Unit (U1) then
         return True;

      --  If both are waiting bodies, then prefer the one whose spec is
      --  less recently elaborated. Consider the following:

      --     spec of A
      --     spec of B
      --     body of A or B?

      --  The normal waiting body preference would have placed the body of
      --  A before the spec of B if it could. Since it could not, there it
      --  must be the case that A depends on B. It is therefore a good idea
      --  to put the body of B last so that if there is an elaboration order
      --  problem, we will find it (that's what horrible order is about)

      elsif Is_Waiting_Body (U1) and then Is_Waiting_Body (U2) then
         return
           UNR.Table (Corresponding_Spec (U1)).Elab_Position <
           UNR.Table (Corresponding_Spec (U2)).Elab_Position;
      end if;

      --  Remaining choice rules are disabled by Debug flag -do

      if not Debug_Flag_O then

         --  The following deal with the case of specs which have been marked
         --  as Elaborate_Body_Desirable. In the normal case, we generally want
         --  to delay the elaboration of these specs as long as possible, so
         --  that bodies have better chance of being elaborated closer to the
         --  specs. Worse_Choice as usual wants to do the opposite and
         --  elaborate such specs as early as possible.

         --  If we have two units, one of which is a spec for which this flag
         --  is set, and the other is not, we normally prefer to delay the spec
         --  for which the flag is set, and so Worse_Choice does the opposite.

         if not UT1.Elaborate_Body_Desirable
           and then UT2.Elaborate_Body_Desirable
         then
            return False;

         elsif not UT2.Elaborate_Body_Desirable
           and then UT1.Elaborate_Body_Desirable
         then
            return True;

            --  If we have two specs that are both marked as Elaborate_Body
            --  desirable, we normally prefer the one whose body is nearer to
            --  being able to be elaborated, based on the Num_Pred count. This
            --  helps to ensure bodies are as close to specs as possible. As
            --  usual, Worse_Choice does the opposite.

         elsif UT1.Elaborate_Body_Desirable
           and then UT2.Elaborate_Body_Desirable
         then
            return UNR.Table (Corresponding_Body (U1)).Num_Pred >=
              UNR.Table (Corresponding_Body (U2)).Num_Pred;
         end if;
      end if;

      --  If we fall through, it means that no preference rule applies, so we
      --  use alphabetical order to at least give a deterministic result. Since
      --  Worse_Choice is in the business of stirring up the order, we will
      --  use reverse alphabetical ordering.

      return Uname_Less (UT2.Uname, UT1.Uname);
   end Worse_Choice;

   ------------------------
   -- Write_Dependencies --
   ------------------------

   procedure Write_Dependencies is
   begin
      if not Zero_Formatting then
         Write_Eol;
         Write_Str ("                 ELABORATION ORDER DEPENDENCIES");
         Write_Eol;
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
                    ("           which must be elaborated " &
                     "along with its spec:",
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

end Binde;

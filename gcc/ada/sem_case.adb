------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ C A S E                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1996-2024, Free Software Foundation, Inc.         --
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

with Atree;          use Atree;
with Einfo;          use Einfo;
with Einfo.Entities; use Einfo.Entities;
with Einfo.Utils;    use Einfo.Utils;
with Elists;         use Elists;
with Errout;         use Errout;
with Namet;          use Namet;
with Nlists;         use Nlists;
with Nmake;          use Nmake;
with Opt;            use Opt;
with Sem;            use Sem;
with Sem_Aux;        use Sem_Aux;
with Sem_Eval;       use Sem_Eval;
with Sem_Res;        use Sem_Res;
with Sem_Util;       use Sem_Util;
with Sem_Type;       use Sem_Type;
with Snames;         use Snames;
with Stand;          use Stand;
with Sinfo;          use Sinfo;
with Sinfo.Nodes;    use Sinfo.Nodes;
with Sinfo.Utils;    use Sinfo.Utils;
with Stringt;        use Stringt;
with Table;
with Tbuild;         use Tbuild;
with Uintp;          use Uintp;
with Warnsw;         use Warnsw;

with Ada.Unchecked_Deallocation;

with GNAT.Heap_Sort_G;
with GNAT.Sets;

package body Sem_Case is

   type Choice_Bounds is record
      Lo   : Node_Id;
      Hi   : Node_Id;
      Node : Node_Id;
   end record;
   --  Represent one choice bounds entry with Lo and Hi values, Node points
   --  to the choice node itself.

   type Choice_Table_Type is array (Nat range <>) of Choice_Bounds;
   --  Table type used to sort the choices present in a case statement or
   --  record variant. The actual entries are stored in 1 .. Last, but we
   --  have a 0 entry for use in sorting.

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Check_Choice_Set
     (Choice_Table   : in out Choice_Table_Type;
      Bounds_Type    : Entity_Id;
      Subtyp         : Entity_Id;
      Others_Present : Boolean;
      Case_Node      : Node_Id);
   --  This is the procedure which verifies that a set of case alternatives
   --  or record variant choices has no duplicates, and covers the range
   --  specified by Bounds_Type. Choice_Table contains the discrete choices
   --  to check. These must start at position 1.
   --
   --  Furthermore Choice_Table (0) must exist. This element is used by
   --  the sorting algorithm as a temporary. Others_Present is a flag
   --  indicating whether or not an Others choice is present. Finally
   --  Msg_Sloc gives the source location of the construct containing the
   --  choices in the Choice_Table.
   --
   --  Bounds_Type is the type whose range must be covered by the alternatives
   --
   --  Subtyp is the subtype of the expression. If its bounds are nonstatic
   --  the alternatives must cover its base type.

   function Choice_Image (Value : Uint; Ctype : Entity_Id) return Name_Id;
   --  Given a Pos value of enumeration type Ctype, returns the name
   --  ID of an appropriate string to be used in error message output.

   function Has_Static_Discriminant_Constraint
     (Subtyp : Entity_Id) return Boolean;
   --  Returns True if the given subtype is subject to a discriminant
   --  constraint and at least one of the constraint values is nonstatic.

   package Composite_Case_Ops is

      Simplified_Composite_Coverage_Rules : constant Boolean := True;
      --  Indicates that, as a temporary stopgap, we implement
      --  simpler coverage-checking rules when casing on a
      --  composite selector:
      --     1) Require that an Others choice must be given, regardless
      --        of whether all possible values are covered explicitly.
      --     2) No legality checks regarding overlapping choices.

      function Box_Value_Required (Subtyp : Entity_Id) return Boolean;
      --  If result is True, then the only allowed value (in a choice
      --  aggregate) for a component of this (sub)type is a box. This rule
      --  means that such a component can be ignored in case alternative
      --  selection. This in turn implies that it is ok if the component
      --  type doesn't meet the usual restrictions, such as not being an
      --  access/task/protected type, since nobody is going to look
      --  at it.

      function Choice_Count (Alternatives : List_Id) return Nat;
      --  The sum of the number of choices for each alternative in the given
      --  list.

      function Normalized_Case_Expr_Type
        (Case_Statement : Node_Id) return Entity_Id;
      --  Usually returns the Etype of the selector expression of the
      --  case statement. However, in the case of a constrained composite
      --  subtype with a nonstatic constraint, returns the unconstrained
      --  base type.

      function Scalar_Part_Count (Subtyp : Entity_Id) return Nat;
      --  Given the composite type Subtyp of a case selector, returns the
      --  number of scalar parts in an object of this type. This is the
      --  dimensionality of the associated Cartesian product space.

      package Array_Case_Ops is
         function Array_Choice_Length (Choice : Node_Id) return Nat;
         --  Given a choice expression of an array type, returns its length.

         function Unconstrained_Array_Effective_Length
           (Array_Type : Entity_Id; Case_Statement : Node_Id) return Nat;
         --  If the nominal subtype of the case selector is unconstrained,
         --  then use the length of the longest choice of the case statement.
         --  Components beyond that index value will not influence the case
         --  selection decision.

         function Unconstrained_Array_Scalar_Part_Count
           (Array_Type : Entity_Id; Case_Statement : Node_Id) return Nat;
         --  Same as Scalar_Part_Count except that the value used for the
         --  "length" of the array subtype being cased on is determined by
         --  calling Unconstrained_Array_Effective_Length.
      end Array_Case_Ops;

      generic
         Case_Statement : Node_Id;
      package Choice_Analysis is

         use Array_Case_Ops;

         type Alternative_Id is
           new Int range 1 .. List_Length (Alternatives (Case_Statement));
         type Choice_Id is
           new Int range 1 .. Choice_Count (Alternatives (Case_Statement));

         Case_Expr_Type : constant Entity_Id :=
           Normalized_Case_Expr_Type (Case_Statement);

         Unconstrained_Array_Case : constant Boolean :=
           Is_Array_Type (Case_Expr_Type)
             and then not Is_Constrained (Case_Expr_Type);

         --  If Unconstrained_Array_Case is True, choice lengths may differ:
         --    when "Aaa" | "Bb" | "C" | "" =>
         --
         --  Strictly speaking, the name "Unconstrained_Array_Case" is
         --  slightly imprecise; a subtype with a nonstatic constraint is
         --  also treated as unconstrained (see Normalize_Case_Expr_Type).

         type Part_Id is new Int range
           1 .. (if Unconstrained_Array_Case
                 then Unconstrained_Array_Scalar_Part_Count
                        (Case_Expr_Type, Case_Statement)
                 else Scalar_Part_Count (Case_Expr_Type));

         type Discrete_Range_Info is
           record
              Low, High : Uint;
           end record;
         function "=" (X, Y : Discrete_Range_Info) return Boolean is abstract;
         --  Here (and below), we don't use "=", which is a good thing,
         --  because it wouldn't work, because the user-defined "=" on
         --  Uint does not compose according to Ada rules.

         type Composite_Range_Info is array (Part_Id) of Discrete_Range_Info;
         function "=" (X, Y : Composite_Range_Info) return Boolean is abstract;

         type Choice_Range_Info (Is_Others : Boolean := False) is
           record
              case Is_Others is
                 when False =>
                    Ranges : Composite_Range_Info;
                 when True =>
                    null;
              end case;
           end record;
         pragma Annotate (CodePeer, False_Positive, "raise exception",
                          "function is abstract, hence never called");
         function "=" (X, Y : Choice_Range_Info) return Boolean is abstract;

         type Choices_Range_Info is array (Choice_Id) of Choice_Range_Info;

         package Value_Sets is

            type Value_Set is private;
            --  A set of points in the Cartesian product space defined
            --  by the composite type of the case selector.
            --  Implemented as an access type.

            type Set_Comparison is
              (Disjoint, Equal, Contains, Contained_By, Overlaps);

            function Compare (S1, S2 : Value_Set) return Set_Comparison;
            --  If either argument (or both) is empty, result is Disjoint.
            --  Otherwise, result is Equal if the two sets are equal.

            Empty : constant Value_Set;

            function Matching_Values
              (Info : Composite_Range_Info) return Value_Set;
            --  The Cartesian product of the given array of ranges
            --  (excluding any values outside the Cartesian product of the
            --  component ranges).

            procedure Union (Target : in out Value_Set; Source : Value_Set);
            --  Add elements of Source into Target

            procedure Remove (Target : in out Value_Set; Source : Value_Set);
            --  Remove elements of Source from Target

            function Complement_Is_Empty (Set : Value_Set) return Boolean;
            --  Return True iff the set is "maximal", in the sense that it
            --  includes every value in the Cartesian product of the
            --  component ranges.

            procedure Free_Value_Sets;
            --  Reclaim storage associated with implementation of this package.

         private
            type Value_Set is new Natural;
            --  An index for a table that will be declared in the package body.

            Empty : constant Value_Set := 0;

         end Value_Sets;

         type Single_Choice_Info (Is_Others : Boolean := False) is
           record
              Alternative : Alternative_Id;
              case Is_Others is
                 when False =>
                    Matches : Value_Sets.Value_Set;
                 when True =>
                    null;
              end case;
           end record;

         type Choices_Info is array (Choice_Id) of Single_Choice_Info;

         function Analysis return Choices_Info;
         --  Parse the case choices in order to determine the set of
         --  matching values associated with each choice.

         type Bound_Values is array (Positive range <>) of Node_Id;

      end Choice_Analysis;
   end Composite_Case_Ops;

   procedure Expand_Others_Choice
     (Case_Table    : Choice_Table_Type;
      Others_Choice : Node_Id;
      Choice_Type   : Entity_Id);
   --  The case table is the table generated by a call to Check_Choices
   --  (with just 1 .. Last_Choice entries present). Others_Choice is a
   --  pointer to the N_Others_Choice node (this routine is only called if
   --  an others choice is present), and Choice_Type is the discrete type
   --  of the bounds. The effect of this call is to analyze the cases and
   --  determine the set of values covered by others. This choice list is
   --  set in the Others_Discrete_Choices field of the N_Others_Choice node.

   ----------------------
   -- Check_Choice_Set --
   ----------------------

   procedure Check_Choice_Set
     (Choice_Table   : in out Choice_Table_Type;
      Bounds_Type    : Entity_Id;
      Subtyp         : Entity_Id;
      Others_Present : Boolean;
      Case_Node      : Node_Id)
   is
      Predicate_Error : Boolean := False;
      --  Flag to prevent cascaded errors when a static predicate is known to
      --  be violated by one choice.

      Num_Choices : constant Nat := Choice_Table'Last;

      procedure Check_Against_Predicate
        (Pred    : in out Node_Id;
         Choice  : Choice_Bounds;
         Prev_Lo : in out Uint;
         Prev_Hi : in out Uint;
         Error   : in out Boolean);
      --  Determine whether a choice covers legal values as defined by a static
      --  predicate set. Pred is a static predicate range. Choice is the choice
      --  to be examined. Prev_Lo and Prev_Hi are the bounds of the previous
      --  choice that covered a predicate set. Error denotes whether the check
      --  found an illegal intersection.

      procedure Check_Duplicates;
      --  Check for duplicate choices, and call Dup_Choice if there are any
      --  such errors. Note that predicates are irrelevant here.

      procedure Dup_Choice (Lo, Hi : Uint; C : Node_Id);
      --  Post message "duplication of choice value(s) bla bla at xx". Message
      --  is posted at location C. Caller sets Error_Msg_Sloc for xx.

      procedure Explain_Non_Static_Bound;
      --  Called when we find a nonstatic bound, requiring the base type to
      --  be covered. Provides where possible a helpful explanation of why the
      --  bounds are nonstatic, since this is not always obvious.

      function Lt_Choice (C1, C2 : Natural) return Boolean;
      --  Comparison routine for comparing Choice_Table entries. Use the lower
      --  bound of each Choice as the key.

      procedure Missing_Choice (Value1 : Node_Id; Value2 : Node_Id);
      procedure Missing_Choice (Value1 : Node_Id; Value2 : Uint);
      procedure Missing_Choice (Value1 : Uint;    Value2 : Node_Id);
      procedure Missing_Choice (Value1 : Uint;    Value2 : Uint);
      --  Issue an error message indicating that there are missing choices,
      --  followed by the image of the missing choices themselves which lie
      --  between Value1 and Value2 inclusive.

      procedure Missing_Choices (Pred : Node_Id; Prev_Hi : Uint);
      --  Emit an error message for each non-covered static predicate set.
      --  Prev_Hi denotes the upper bound of the last choice covering a set.

      procedure Move_Choice (From : Natural; To : Natural);
      --  Move routine for sorting the Choice_Table

      package Sorting is new GNAT.Heap_Sort_G (Move_Choice, Lt_Choice);

      -----------------------------
      -- Check_Against_Predicate --
      -----------------------------

      procedure Check_Against_Predicate
        (Pred    : in out Node_Id;
         Choice  : Choice_Bounds;
         Prev_Lo : in out Uint;
         Prev_Hi : in out Uint;
         Error   : in out Boolean)
      is
         procedure Illegal_Range
           (Loc : Source_Ptr;
            Lo  : Uint;
            Hi  : Uint);
         --  Emit an error message regarding a choice that clashes with the
         --  legal static predicate sets. Loc is the location of the choice
         --  that introduced the illegal range. Lo .. Hi is the range.

         function Inside_Range
           (Lo  : Uint;
            Hi  : Uint;
            Val : Uint) return Boolean;
         --  Determine whether position Val within a discrete type is within
         --  the range Lo .. Hi inclusive.

         -------------------
         -- Illegal_Range --
         -------------------

         procedure Illegal_Range
           (Loc : Source_Ptr;
            Lo  : Uint;
            Hi  : Uint)
         is
         begin
            Error_Msg_Name_1 := Chars (Bounds_Type);

            --  Single value

            if Lo = Hi then
               if Is_Integer_Type (Bounds_Type) then
                  Error_Msg_Uint_1 := Lo;
                  Error_Msg ("static predicate on % excludes value ^!", Loc);
               else
                  Error_Msg_Name_2 := Choice_Image (Lo, Bounds_Type);
                  Error_Msg ("static predicate on % excludes value %!", Loc);
               end if;

            --  Range

            else
               if Is_Integer_Type (Bounds_Type) then
                  Error_Msg_Uint_1 := Lo;
                  Error_Msg_Uint_2 := Hi;
                  Error_Msg
                    ("static predicate on % excludes range ^ .. ^!", Loc);
               else
                  Error_Msg_Name_2 := Choice_Image (Lo, Bounds_Type);
                  Error_Msg_Name_3 := Choice_Image (Hi, Bounds_Type);
                  Error_Msg
                    ("static predicate on % excludes range % .. %!", Loc);
               end if;
            end if;
         end Illegal_Range;

         ------------------
         -- Inside_Range --
         ------------------

         function Inside_Range
           (Lo  : Uint;
            Hi  : Uint;
            Val : Uint) return Boolean
         is
         begin
            return Lo <= Val and then Val <= Hi;
         end Inside_Range;

         --  Local variables

         Choice_Hi : constant Uint := Expr_Value (Choice.Hi);
         Choice_Lo : constant Uint := Expr_Value (Choice.Lo);
         Loc       : Source_Ptr;
         LocN      : Node_Id;
         Next_Hi   : Uint;
         Next_Lo   : Uint;
         Pred_Hi   : Uint;
         Pred_Lo   : Uint;

      --  Start of processing for Check_Against_Predicate

      begin
         --  Find the proper error message location

         if Present (Choice.Node) then
            LocN := Choice.Node;
         else
            LocN := Case_Node;
         end if;

         Loc := Sloc (LocN);

         if Present (Pred) then
            Pred_Lo := Expr_Value (Low_Bound  (Pred));
            Pred_Hi := Expr_Value (High_Bound (Pred));

         --  Previous choices managed to satisfy all static predicate sets

         else
            Illegal_Range (Loc, Choice_Lo, Choice_Hi);
            Error := True;
            return;
         end if;

         --  Step 1: Ignore duplicate choices, other than to set the flag,
         --  because these were already detected by Check_Duplicates.

         if Inside_Range (Choice_Lo, Choice_Hi, Prev_Lo)
           or else  Inside_Range (Choice_Lo, Choice_Hi, Prev_Hi)
         then
            Error := True;

         --  Step 2: Detect full coverage

         --  Choice_Lo    Choice_Hi
         --  +============+
         --  Pred_Lo      Pred_Hi

         elsif Choice_Lo = Pred_Lo and then Choice_Hi = Pred_Hi then
            Prev_Lo := Choice_Lo;
            Prev_Hi := Choice_Hi;
            Next (Pred);

         --  Step 3: Detect all cases where a choice mentions values that are
         --  not part of the static predicate sets.

         --  Choice_Lo   Choice_Hi   Pred_Lo   Pred_Hi
         --  +-----------+ . . . . . +=========+
         --   ^ illegal ^

         elsif Choice_Lo < Pred_Lo and then Choice_Hi < Pred_Lo then
            Illegal_Range (Loc, Choice_Lo, Choice_Hi);
            Error := True;

         --  Choice_Lo   Pred_Lo   Choice_Hi   Pred_Hi
         --  +-----------+=========+===========+
         --   ^ illegal ^

         elsif Choice_Lo < Pred_Lo
           and then Inside_Range (Pred_Lo, Pred_Hi, Choice_Hi)
         then
            Illegal_Range (Loc, Choice_Lo, Pred_Lo - 1);
            Error := True;

         --  Pred_Lo   Pred_Hi   Choice_Lo   Choice_Hi
         --  +=========+ . . . . +-----------+
         --                       ^ illegal ^

         elsif Pred_Lo < Choice_Lo and then Pred_Hi < Choice_Lo then
            if Others_Present then

               --  Current predicate set is covered by others clause.

               null;

            else
               Missing_Choice (Pred_Lo, Pred_Hi);
               Error := True;
            end if;

            --  There may be several static predicate sets between the current
            --  one and the choice. Inspect the next static predicate set.

            Next (Pred);
            Check_Against_Predicate
              (Pred    => Pred,
               Choice  => Choice,
               Prev_Lo => Prev_Lo,
               Prev_Hi => Prev_Hi,
               Error   => Error);

         --  Pred_Lo   Choice_Lo   Pred_Hi     Choice_Hi
         --  +=========+===========+-----------+
         --                         ^ illegal ^

         elsif Pred_Hi < Choice_Hi
           and then Inside_Range (Pred_Lo, Pred_Hi, Choice_Lo)
         then
            Next (Pred);

            --  The choice may fall in a static predicate set. If this is the
            --  case, avoid mentioning legal values in the error message.

            if Present (Pred) then
               Next_Lo := Expr_Value (Low_Bound  (Pred));
               Next_Hi := Expr_Value (High_Bound (Pred));

               --  The next static predicate set is to the right of the choice

               if Choice_Hi < Next_Lo and then Choice_Hi < Next_Hi then
                  Illegal_Range (Loc, Pred_Hi + 1, Choice_Hi);
               else
                  Illegal_Range (Loc, Pred_Hi + 1, Next_Lo - 1);
               end if;
            else
               Illegal_Range (Loc, Pred_Hi + 1, Choice_Hi);
            end if;

            Error := True;

         --  Choice_Lo   Pred_Lo   Pred_Hi     Choice_Hi
         --  +-----------+=========+-----------+
         --   ^ illegal ^           ^ illegal ^

         --  Emit an error on the low gap, disregard the upper gap

         elsif Choice_Lo < Pred_Lo and then Pred_Hi < Choice_Hi then
            Illegal_Range (Loc, Choice_Lo, Pred_Lo - 1);
            Error := True;

         --  Step 4: Detect all cases of partial or missing coverage

         --  Pred_Lo   Choice_Lo  Choice_Hi   Pred_Hi
         --  +=========+==========+===========+
         --   ^  gap  ^            ^   gap   ^

         else
            --  An "others" choice covers all gaps

            if Others_Present then
               Prev_Lo := Choice_Lo;
               Prev_Hi := Choice_Hi;

               --  Check whether predicate set is fully covered by choice

               if Pred_Hi = Choice_Hi then
                  Next (Pred);
               end if;

            --  Choice_Lo   Choice_Hi   Pred_Hi
            --  +===========+===========+
            --  Pred_Lo      ^   gap   ^

            --  The upper gap may be covered by a subsequent choice

            elsif Pred_Lo = Choice_Lo then
               Prev_Lo := Choice_Lo;
               Prev_Hi := Choice_Hi;

            --  Pred_Lo     Prev_Hi   Choice_Lo   Choice_Hi   Pred_Hi
            --  +===========+=========+===========+===========+
            --   ^ covered ^ ^  gap  ^

            else pragma Assert (Pred_Lo < Choice_Lo);

               --  A previous choice covered the gap up to the current choice

               if Prev_Hi = Choice_Lo - 1 then
                  Prev_Lo := Choice_Lo;
                  Prev_Hi := Choice_Hi;

                  if Choice_Hi = Pred_Hi then
                     Next (Pred);
                  end if;

               --  The previous choice did not intersect with the current
               --  static predicate set.

               elsif Prev_Hi < Pred_Lo then
                  Missing_Choice (Pred_Lo, Choice_Lo - 1);
                  Error := True;

               --  The previous choice covered part of the static predicate set
               --  but there is a gap after Prev_Hi.

               else
                  Missing_Choice (Prev_Hi + 1, Choice_Lo - 1);
                  Error := True;
               end if;
            end if;
         end if;
      end Check_Against_Predicate;

      ----------------------
      -- Check_Duplicates --
      ----------------------

      procedure Check_Duplicates is
         Choice      : Node_Id;
         Choice_Hi   : Uint;
         Choice_Lo   : Uint;
         Prev_Choice : Node_Id := Empty;
         Prev_Hi     : Uint;

      begin
         Prev_Hi := Expr_Value (Choice_Table (1).Hi);

         for Outer_Index in 2 .. Num_Choices loop
            Choice_Lo := Expr_Value (Choice_Table (Outer_Index).Lo);
            Choice_Hi := Expr_Value (Choice_Table (Outer_Index).Hi);

            --  Choices overlap; this is an error

            if Choice_Lo <= Prev_Hi then
               Choice := Choice_Table (Outer_Index).Node;

               --  Find first previous choice that overlaps

               for Inner_Index in 1 .. Outer_Index - 1 loop
                  if Choice_Lo <=
                       Expr_Value (Choice_Table (Inner_Index).Hi)
                  then
                     Prev_Choice := Choice_Table (Inner_Index).Node;
                     exit;
                  end if;
               end loop;

               pragma Assert (Present (Prev_Choice));

               if Sloc (Prev_Choice) <= Sloc (Choice) then
                  Error_Msg_Sloc := Sloc (Prev_Choice);
                  Dup_Choice (Choice_Lo, UI_Min (Choice_Hi, Prev_Hi), Choice);
               else
                  Error_Msg_Sloc := Sloc (Choice);
                  Dup_Choice
                    (Choice_Lo, UI_Min (Choice_Hi, Prev_Hi), Prev_Choice);
               end if;
            end if;

            if Choice_Hi > Prev_Hi then
               Prev_Hi := Choice_Hi;
            end if;
         end loop;
      end Check_Duplicates;

      ----------------
      -- Dup_Choice --
      ----------------

      procedure Dup_Choice (Lo, Hi : Uint; C : Node_Id) is
      begin
         --  In some situations, we call this with a null range, and obviously
         --  we don't want to complain in this case.

         if Lo > Hi then
            return;
         end if;

         --  Case of only one value that is duplicated

         if Lo = Hi then

            --  Integer type

            if Is_Integer_Type (Bounds_Type) then

               --  We have an integer value, Lo, but if the given choice
               --  placement is a constant with that value, then use the
               --  name of that constant instead in the message:

               if Nkind (C) = N_Identifier
                 and then Compile_Time_Known_Value (C)
                 and then Expr_Value (C) = Lo
               then
                  Error_Msg_N
                    ("duplication of choice value: &#!", Original_Node (C));

               --  Not that special case, so just output the integer value

               else
                  Error_Msg_Uint_1 := Lo;
                  Error_Msg_N
                    ("duplication of choice value: ^#!", Original_Node (C));
               end if;

            --  Enumeration type

            else
               Error_Msg_Name_1 := Choice_Image (Lo, Bounds_Type);
               Error_Msg_N
                 ("duplication of choice value: %#!", Original_Node (C));
            end if;

         --  More than one choice value, so print range of values

         else
            --  Integer type

            if Is_Integer_Type (Bounds_Type) then

               --  Similar to the above, if C is a range of known values which
               --  match Lo and Hi, then use the names. We have to go to the
               --  original nodes, since the values will have been rewritten
               --  to their integer values.

               if Nkind (C) = N_Range
                 and then Nkind (Original_Node (Low_Bound  (C))) = N_Identifier
                 and then Nkind (Original_Node (High_Bound (C))) = N_Identifier
                 and then Compile_Time_Known_Value (Low_Bound (C))
                 and then Compile_Time_Known_Value (High_Bound (C))
                 and then Expr_Value (Low_Bound (C))  = Lo
                 and then Expr_Value (High_Bound (C)) = Hi
               then
                  Error_Msg_Node_2 := Original_Node (High_Bound (C));
                  Error_Msg_N
                    ("duplication of choice values: & .. &#!",
                     Original_Node (Low_Bound (C)));

               --  Not that special case, output integer values

               else
                  Error_Msg_Uint_1 := Lo;
                  Error_Msg_Uint_2 := Hi;
                  Error_Msg_N
                    ("duplication of choice values: ^ .. ^#!",
                     Original_Node (C));
               end if;

            --  Enumeration type

            else
               Error_Msg_Name_1 := Choice_Image (Lo, Bounds_Type);
               Error_Msg_Name_2 := Choice_Image (Hi, Bounds_Type);
               Error_Msg_N
                 ("duplication of choice values: % .. %#!", Original_Node (C));
            end if;
         end if;
      end Dup_Choice;

      ------------------------------
      -- Explain_Non_Static_Bound --
      ------------------------------

      procedure Explain_Non_Static_Bound is
         Expr : Node_Id;

      begin
         if Nkind (Case_Node) = N_Variant_Part then
            Expr := Name (Case_Node);
         else
            Expr := Expression (Case_Node);
         end if;

         if Bounds_Type /= Subtyp then

            --  If the case is a variant part, the expression is given by the
            --  discriminant itself, and the bounds are the culprits.

            if Nkind (Case_Node) = N_Variant_Part then
               Error_Msg_NE
                 ("bounds of & are not static, "
                  & "alternatives must cover base type!", Expr, Expr);

            --  If this is a case statement, the expression may be nonstatic
            --  or else the subtype may be at fault.

            elsif Is_Entity_Name (Expr) then
               Error_Msg_NE
                 ("bounds of & are not static, "
                  & "alternatives must cover base type!", Expr, Expr);

            else
               Error_Msg_N
                 ("subtype of expression is not static, "
                  & "alternatives must cover base type!", Expr);
            end if;

         --  Otherwise the expression is not static, even if the bounds of the
         --  type are, or else there are missing alternatives. If both, the
         --  additional information may be redundant but harmless. Examine
         --  whether original node is an entity, because it may have been
         --  constant-folded to a literal if value is known.

         elsif not Is_Entity_Name (Original_Node (Expr)) then
            Error_Msg_N
              ("subtype of expression is not static, "
               & "alternatives must cover base type!", Expr);
         end if;
      end Explain_Non_Static_Bound;

      ---------------
      -- Lt_Choice --
      ---------------

      function Lt_Choice (C1, C2 : Natural) return Boolean is
      begin
         return
           Expr_Value (Choice_Table (Nat (C1)).Lo)
             <
           Expr_Value (Choice_Table (Nat (C2)).Lo);
      end Lt_Choice;

      --------------------
      -- Missing_Choice --
      --------------------

      procedure Missing_Choice (Value1 : Node_Id; Value2 : Node_Id) is
      begin
         Missing_Choice (Expr_Value (Value1), Expr_Value (Value2));
      end Missing_Choice;

      procedure Missing_Choice (Value1 : Node_Id; Value2 : Uint) is
      begin
         Missing_Choice (Expr_Value (Value1), Value2);
      end Missing_Choice;

      procedure Missing_Choice (Value1 : Uint; Value2 : Node_Id) is
      begin
         Missing_Choice (Value1, Expr_Value (Value2));
      end Missing_Choice;

      --------------------
      -- Missing_Choice --
      --------------------

      procedure Missing_Choice (Value1 : Uint; Value2 : Uint) is
      begin
         --  AI05-0188 : within an instance the non-others choices do not have
         --  to belong to the actual subtype.

         if Ada_Version >= Ada_2012 and then In_Instance then
            return;

         --  In some situations, we call this with a null range, and obviously
         --  we don't want to complain in this case.

         elsif Value1 > Value2 then
            return;

         --  If predicate is already known to be violated, do not check for
         --  coverage error, to prevent cascaded messages.

         elsif Predicate_Error then
            return;
         end if;

         --  Case of only one value that is missing

         if Value1 = Value2 then
            if Is_Integer_Type (Bounds_Type) then
               Error_Msg_Uint_1 := Value1;
               Error_Msg_N ("missing case value: ^!", Case_Node);
            else
               Error_Msg_Name_1 := Choice_Image (Value1, Bounds_Type);
               Error_Msg_N ("missing case value: %!", Case_Node);
            end if;

         --  More than one choice value, so print range of values

         else
            if Is_Integer_Type (Bounds_Type) then
               Error_Msg_Uint_1 := Value1;
               Error_Msg_Uint_2 := Value2;
               Error_Msg_N ("missing case values: ^ .. ^!", Case_Node);
            else
               Error_Msg_Name_1 := Choice_Image (Value1, Bounds_Type);
               Error_Msg_Name_2 := Choice_Image (Value2, Bounds_Type);
               Error_Msg_N ("missing case values: % .. %!", Case_Node);
            end if;
         end if;
      end Missing_Choice;

      ---------------------
      -- Missing_Choices --
      ---------------------

      procedure Missing_Choices (Pred : Node_Id; Prev_Hi : Uint) is
         Hi  : Uint;
         Lo  : Uint;
         Set : Node_Id;

      begin
         Set := Pred;
         while Present (Set) loop
            Lo := Expr_Value (Low_Bound (Set));
            Hi := Expr_Value (High_Bound (Set));

            --  A choice covered part of a static predicate set

            if Lo <= Prev_Hi and then Prev_Hi < Hi then
               Missing_Choice (Prev_Hi + 1, Hi);

            else
               Missing_Choice (Lo, Hi);
            end if;

            Next (Set);
         end loop;
      end Missing_Choices;

      -----------------
      -- Move_Choice --
      -----------------

      procedure Move_Choice (From : Natural; To : Natural) is
      begin
         Choice_Table (Nat (To)) := Choice_Table (Nat (From));
      end Move_Choice;

      --  Local variables

      Bounds_Hi     : constant Node_Id := Type_High_Bound (Bounds_Type);
      Bounds_Lo     : constant Node_Id := Type_Low_Bound  (Bounds_Type);
      Has_Predicate : constant Boolean :=
                        Is_OK_Static_Subtype (Bounds_Type)
                          and then Has_Static_Predicate (Bounds_Type);

      Choice_Hi   : Uint;
      Choice_Lo   : Uint;
      Pred        : Node_Id;
      Prev_Lo     : Uint;
      Prev_Hi     : Uint;

   --  Start of processing for Check_Choice_Set

   begin
      --  If the case is part of a predicate aspect specification, do not
      --  recheck it against itself.

      if Present (Parent (Case_Node))
        and then Nkind (Parent (Case_Node)) = N_Aspect_Specification
      then
         return;
      end if;

      --  Choice_Table must start at 0 which is an unused location used by the
      --  sorting algorithm. However the first valid position for a discrete
      --  choice is 1.

      pragma Assert (Choice_Table'First = 0);

      --  The choices do not cover the base range. Emit an error if "others" is
      --  not available and return as there is no need for further processing.

      if Num_Choices = 0 then
         if not Others_Present then
            Missing_Choice (Bounds_Lo, Bounds_Hi);
         end if;

         return;
      end if;

      Sorting.Sort (Positive (Choice_Table'Last));

      --  First check for duplicates. This involved the choices; predicates, if
      --  any, are irrelevant.

      Check_Duplicates;

      --  Then check for overlaps

      --  If the subtype has a static predicate, the predicate defines subsets
      --  of legal values and requires finer-grained analysis.

      --  Note that in GNAT the predicate is considered static if the predicate
      --  expression is static, independently of whether the aspect mentions
      --  Static explicitly.

      if Has_Predicate then
         Pred := First (Static_Discrete_Predicate (Bounds_Type));

         --  Make initial value smaller than 'First of type, so that first
         --  range comparison succeeds. This applies both to integer types
         --  and to enumeration types.

         Prev_Lo := Expr_Value (Type_Low_Bound (Bounds_Type)) - 1;
         Prev_Hi := Prev_Lo;

         declare
            Error : Boolean := False;
         begin
            for Index in 1 .. Num_Choices loop
               Check_Against_Predicate
                 (Pred    => Pred,
                  Choice  => Choice_Table (Index),
                  Prev_Lo => Prev_Lo,
                  Prev_Hi => Prev_Hi,
                  Error   => Error);

               --  The analysis detected an illegal intersection between a
               --  choice and a static predicate set. Do not examine other
               --  choices unless all errors are requested.

               if Error then
                  Predicate_Error := True;

                  if not All_Errors_Mode then
                     return;
                  end if;
               end if;
            end loop;
         end;

         if Predicate_Error then
            return;
         end if;

         --  The choices may legally cover some of the static predicate sets,
         --  but not all. Emit an error for each non-covered set.

         if not Others_Present then
            Missing_Choices (Pred, Prev_Hi);
         end if;

      --  Default analysis

      else
         Choice_Lo := Expr_Value (Choice_Table (1).Lo);
         Choice_Hi := Expr_Value (Choice_Table (1).Hi);
         Prev_Hi   := Choice_Hi;

         if not Others_Present and then Expr_Value (Bounds_Lo) < Choice_Lo then
            Missing_Choice (Bounds_Lo, Choice_Lo - 1);

            --  If values are missing outside of the subtype, add explanation.
            --  No additional message if only one value is missing.

            if Expr_Value (Bounds_Lo) < Choice_Lo - 1 then
               Explain_Non_Static_Bound;
            end if;
         end if;

         for Index in 2 .. Num_Choices loop
            Choice_Lo := Expr_Value (Choice_Table (Index).Lo);
            Choice_Hi := Expr_Value (Choice_Table (Index).Hi);

            if Choice_Lo > Prev_Hi + 1 and then not Others_Present then
               Missing_Choice (Prev_Hi + 1, Choice_Lo - 1);
            end if;

            if Choice_Hi > Prev_Hi then
               Prev_Hi := Choice_Hi;
            end if;
         end loop;

         if not Others_Present and then Expr_Value (Bounds_Hi) > Prev_Hi then
            Missing_Choice (Prev_Hi + 1, Bounds_Hi);

            if Expr_Value (Bounds_Hi) > Prev_Hi + 1 then
               Explain_Non_Static_Bound;
            end if;
         end if;
      end if;
   end Check_Choice_Set;

   ------------------
   -- Choice_Image --
   ------------------

   function Choice_Image (Value : Uint; Ctype : Entity_Id) return Name_Id is
      Rtp : constant Entity_Id := Root_Type (Ctype);
      Lit : Entity_Id;
      C   : Int;

   begin
      --  For character, or wide [wide] character. If 7-bit ASCII graphic
      --  range, then build and return appropriate character literal name

      if Is_Standard_Character_Type (Ctype) then
         C := UI_To_Int (Value);

         if C in 16#20# .. 16#7E# then
            Set_Character_Literal_Name (UI_To_CC (Value));
            return Name_Find;
         end if;

      --  For user defined enumeration type, find enum/char literal

      else
         Lit := First_Literal (Rtp);

         for J in 1 .. UI_To_Int (Value) loop
            Next_Literal (Lit);
         end loop;

         --  If enumeration literal, just return its value

         if Nkind (Lit) = N_Defining_Identifier then
            return Chars (Lit);

         --  For character literal, get the name and use it if it is
         --  for a 7-bit ASCII graphic character in 16#20#..16#7E#.

         else
            Get_Decoded_Name_String (Chars (Lit));

            if Name_Len = 3
              and then Name_Buffer (2) in
                Character'Val (16#20#) .. Character'Val (16#7E#)
            then
               return Chars (Lit);
            end if;
         end if;
      end if;

      --  If we fall through, we have a character literal which is not in
      --  the 7-bit ASCII graphic set. For such cases, we construct the
      --  name "type'val(nnn)" where type is the choice type, and nnn is
      --  the pos value passed as an argument to Choice_Image.

      Get_Name_String (Chars (First_Subtype (Ctype)));

      Add_Str_To_Name_Buffer ("'val(");
      UI_Image (Value);
      Add_Str_To_Name_Buffer (UI_Image_Buffer (1 .. UI_Image_Length));
      Add_Char_To_Name_Buffer (')');
      return Name_Find;
   end Choice_Image;

   package body Composite_Case_Ops is

      function Static_Array_Length (Subtyp : Entity_Id) return Nat;
      --  Given a one-dimensional constrained array subtype with
      --  statically known bounds, return its length.

      -------------------------
      -- Static_Array_Length --
      -------------------------

      function Static_Array_Length (Subtyp : Entity_Id) return Nat is
         pragma Assert (Is_Constrained (Subtyp));
         pragma Assert (Number_Dimensions (Subtyp) = 1);
         Index : constant Node_Id := First_Index (Subtyp);
         pragma Assert (Is_OK_Static_Range (Index));
         Lo  : constant Uint := Expr_Value (Low_Bound (Index));
         Hi  : constant Uint := Expr_Value (High_Bound (Index));
         Len : constant Uint := UI_Max (0, (Hi - Lo) + 1);
      begin
         return UI_To_Int (Len);
      end Static_Array_Length;

      ------------------------
      -- Box_Value_Required --
      ------------------------

      function Box_Value_Required (Subtyp : Entity_Id) return Boolean is
         --  Some of these restrictions will be relaxed eventually, but best
         --  to initially err in the direction of being too restrictive.
      begin
         if Has_Predicates (Subtyp) then
            return True;
         elsif Is_Discrete_Type (Subtyp) then
            if not Is_Static_Subtype (Subtyp) then
               return True;
            elsif Is_Enumeration_Type (Subtyp)
               and then Has_Enumeration_Rep_Clause (Subtyp)
               --  Maybe enumeration rep clauses can be ignored here?
            then
               return True;
            end if;
         elsif Is_Array_Type (Subtyp) then
            if Number_Dimensions (Subtyp) /= 1 then
               return True;
            elsif not Is_Constrained (Subtyp) then
               if not Is_Static_Subtype (Etype (First_Index (Subtyp))) then
                  return True;
               end if;
            elsif not Is_OK_Static_Range (First_Index (Subtyp)) then
               return True;
            end if;
         elsif Is_Record_Type (Subtyp) then
            if Has_Discriminants (Subtyp)
              and then Is_Constrained (Subtyp)
              and then not Has_Static_Discriminant_Constraint (Subtyp)
            then
               --  Perhaps treat differently the case where Subtyp is the
               --  subtype of the top-level selector expression, as opposed
               --  to the subtype of some subcomponent thereof.
               return True;
            end if;
         else
            --  Return True for any type that is not a discrete type,
            --  a record type, or an array type.
            return True;
         end if;

         return False;
      end Box_Value_Required;

      ------------------
      -- Choice_Count --
      ------------------

      function Choice_Count (Alternatives : List_Id) return Nat is
         Result : Nat := 0;
         Alt : Node_Id := First (Alternatives);
      begin
         while Present (Alt) loop
            Result := Result + List_Length (Discrete_Choices (Alt));
            Next (Alt);
         end loop;
         return Result;
      end Choice_Count;

      -------------------------------
      -- Normalized_Case_Expr_Type --
      -------------------------------

      function Normalized_Case_Expr_Type
        (Case_Statement : Node_Id) return Entity_Id
      is
         Unnormalized : constant Entity_Id :=
           Etype (Expression (Case_Statement));

         Is_Dynamically_Constrained_Array : constant Boolean :=
           Is_Array_Type (Unnormalized)
             and then Is_Constrained (Unnormalized)
             and then not Has_Static_Array_Bounds (Unnormalized);

         Is_Dynamically_Constrained_Record : constant Boolean :=
           Is_Record_Type (Unnormalized)
             and then Has_Discriminants (Unnormalized)
             and then Is_Constrained (Unnormalized)
             and then not Has_Static_Discriminant_Constraint (Unnormalized);
      begin
         if Is_Dynamically_Constrained_Array
           or Is_Dynamically_Constrained_Record
         then
            return Base_Type (Unnormalized);
         else
            return Unnormalized;
         end if;
      end Normalized_Case_Expr_Type;

      -----------------------
      -- Scalar_Part_Count --
      -----------------------

      function Scalar_Part_Count (Subtyp : Entity_Id) return Nat is
      begin
         if Box_Value_Required (Subtyp) then
            return 0; -- component does not participate in case selection
         elsif Is_Scalar_Type (Subtyp) then
            return 1;
         elsif Is_Array_Type (Subtyp) then
            return Static_Array_Length (Subtyp)
              * Scalar_Part_Count (Component_Type (Subtyp));
         elsif Is_Record_Type (Subtyp) then
            declare
               Result : Nat := 0;
               Comp : Entity_Id := First_Component_Or_Discriminant
                                     (Base_Type (Subtyp));
            begin
               while Present (Comp) loop
                  Result := Result + Scalar_Part_Count (Etype (Comp));
                  Next_Component_Or_Discriminant (Comp);
               end loop;
               return Result;
            end;
         else
            pragma Assert (Serious_Errors_Detected > 0);
            return 0;
         end if;
      end Scalar_Part_Count;

      package body Array_Case_Ops is

         -------------------------
         -- Array_Choice_Length --
         -------------------------

         function Array_Choice_Length (Choice : Node_Id) return Nat is
         begin
            case Nkind (Choice) is
               when N_String_Literal =>
                  return String_Length (Strval (Choice));
               when N_Aggregate =>
                  declare
                     Bounds : constant Node_Id :=
                       Aggregate_Bounds (Choice);
                     pragma Assert (Is_OK_Static_Range (Bounds));
                     Lo     : constant Uint :=
                       Expr_Value (Low_Bound (Bounds));
                     Hi     : constant Uint :=
                       Expr_Value (High_Bound (Bounds));
                     Len : constant Uint := (Hi - Lo) + 1;
                  begin
                     return UI_To_Int (Len);
                  end;
               when N_Has_Entity =>
                  if Present (Entity (Choice))
                    and then Ekind (Entity (Choice)) = E_Constant
                  then
                     return Array_Choice_Length
                              (Expression (Parent (Entity (Choice))));
                  end if;
               when N_Others_Choice =>
                  return 0;
               when others =>
                  null;
            end case;

            if Nkind (Original_Node (Choice))
                 in N_String_Literal | N_Aggregate
            then
               return Array_Choice_Length (Original_Node (Choice));
            end if;

            Error_Msg_N ("Unsupported case choice", Choice);
            return 0;
         end Array_Choice_Length;

         ------------------------------------------
         -- Unconstrained_Array_Effective_Length --
         ------------------------------------------

         function Unconstrained_Array_Effective_Length
           (Array_Type : Entity_Id; Case_Statement : Node_Id) return Nat
         is
            pragma Assert (Is_Array_Type (Array_Type));
            --  Array_Type is otherwise unreferenced for now.

            Result : Nat := 0;
            Alt : Node_Id := First (Alternatives (Case_Statement));
         begin
            while Present (Alt) loop
               declare
                  Choice : Node_Id := First (Discrete_Choices (Alt));
               begin
                  while Present (Choice) loop
                     Result := Nat'Max (Result, Array_Choice_Length (Choice));
                     Next (Choice);
                  end loop;
               end;
               Next (Alt);
            end loop;

            return Result;
         end Unconstrained_Array_Effective_Length;

         -------------------------------------------
         -- Unconstrained_Array_Scalar_Part_Count --
         -------------------------------------------

         function Unconstrained_Array_Scalar_Part_Count
           (Array_Type : Entity_Id; Case_Statement : Node_Id) return Nat
         is
         begin
            --  Add one for the length, which is treated like a discriminant

            return 1 + (Unconstrained_Array_Effective_Length
                          (Array_Type     => Array_Type,
                           Case_Statement => Case_Statement)
                        * Scalar_Part_Count (Component_Type (Array_Type)));
         end Unconstrained_Array_Scalar_Part_Count;

      end Array_Case_Ops;

      package body Choice_Analysis is

         function Component_Bounds_Info return Composite_Range_Info;
         --  Returns the (statically known) bounds for each component.
         --  The selector expression value (or any other value of the type
         --  of the selector expression) can be thought of as a point in the
         --  Cartesian product of these sets.

         function Parse_Choice (Choice : Node_Id;
                                Alt    : Node_Id) return Choice_Range_Info;
         --  Extract Choice_Range_Info from a Choice node

         ---------------------------
         -- Component_Bounds_Info --
         ---------------------------

         function Component_Bounds_Info return Composite_Range_Info is
            Result : Composite_Range_Info;
            Next   : Part_Id := 1;
            Done   : Boolean := False;

            procedure Update_Result (Info : Discrete_Range_Info);
            --  Initialize first remaining uninitialized element of Result.
            --  Also set Next and Done.

            -------------------
            -- Update_Result --
            -------------------

            procedure Update_Result (Info : Discrete_Range_Info) is
            begin
               Result (Next) := Info;
               if Next /= Part_Id'Last then
                  Next := Next + 1;
               else
                  pragma Assert (not Done);
                  Done := True;
               end if;
            end Update_Result;

            procedure Traverse_Discrete_Parts (Subtyp : Entity_Id);
            --  Traverse the given subtype, looking for discrete parts.
            --  For an array subtype of length N, the element subtype
            --  is traversed N times. For a record subtype, traverse
            --  each component's subtype (once). When a discrete part is
            --  found, call Update_Result.

            -----------------------------
            -- Traverse_Discrete_Parts --
            -----------------------------

            procedure Traverse_Discrete_Parts (Subtyp : Entity_Id) is
            begin
               if Box_Value_Required (Subtyp) then
                  return;
               end if;

               if Is_Discrete_Type (Subtyp) then
                  Update_Result
                    ((Low  => Expr_Value (Type_Low_Bound (Subtyp)),
                      High => Expr_Value (Type_High_Bound (Subtyp))));
               elsif Is_Array_Type (Subtyp) then
                  declare
                     Len : Nat;
                  begin
                     if Is_Constrained (Subtyp) then
                        Len := Static_Array_Length (Subtyp);
                     else
                        --  Length will be treated like a discriminant;
                        --  We could compute High more precisely as
                        --    1 + Index_Subtype'Last - Index_Subtype'First
                        --  (we currently require that those bounds be
                        --  static, so this is an option), but only downside of
                        --  overshooting is if somebody wants to omit a
                        --  "when others" choice and exhaustively cover all
                        --  possibilities explicitly.
                        Update_Result
                          ((Low  => Uint_0,
                            High => Uint_2 ** Uint_32));

                        Len := Unconstrained_Array_Effective_Length
                                 (Array_Type     => Subtyp,
                                  Case_Statement => Case_Statement);
                     end if;
                     for I in 1 .. Len loop
                        Traverse_Discrete_Parts (Component_Type (Subtyp));
                     end loop;
                  end;
               elsif Is_Record_Type (Subtyp) then
                  if Has_Static_Discriminant_Constraint (Subtyp) then

                     --  The component range for a constrained discriminant
                     --  is a single value.
                     declare
                        Dc_Elmt : Elmt_Id :=
                          First_Elmt (Discriminant_Constraint (Subtyp));
                        Dc_Value : Uint;
                     begin
                        while Present (Dc_Elmt) loop
                           Dc_Value := Expr_Value (Node (Dc_Elmt));
                           Update_Result ((Low  => Dc_Value,
                                           High => Dc_Value));

                           Next_Elmt (Dc_Elmt);
                        end loop;
                     end;

                     --  Generate ranges for nondiscriminant components.
                     declare
                        Comp : Entity_Id := First_Component
                                              (Base_Type (Subtyp));
                     begin
                        while Present (Comp) loop
                           Traverse_Discrete_Parts (Etype (Comp));
                           Next_Component (Comp);
                        end loop;
                     end;
                  else
                     --  Generate ranges for all components
                     declare
                        Comp : Entity_Id :=
                          First_Component_Or_Discriminant
                            (Base_Type (Subtyp));
                     begin
                        while Present (Comp) loop
                           Traverse_Discrete_Parts (Etype (Comp));
                           Next_Component_Or_Discriminant (Comp);
                        end loop;
                     end;
                  end if;
               else
                  Error_Msg_N
                    ("case selector type having a non-discrete non-record"
                     & "  non-array subcomponent type not implemented",
                     Expression (Case_Statement));
               end if;
            end Traverse_Discrete_Parts;

         begin
            Traverse_Discrete_Parts (Case_Expr_Type);
            pragma Assert (Done or else Serious_Errors_Detected > 0);
            return Result;
         end Component_Bounds_Info;

         Component_Bounds : constant Composite_Range_Info :=
           Component_Bounds_Info;

         package Case_Bindings is

            procedure Note_Binding
              (Comp_Assoc : Node_Id;
               Choice     : Node_Id;
               Alt        : Node_Id);
            --  Note_Binding is called once for each component association
            --  that defines a binding (using either "A => B is X" or
            --  "A => <X>" syntax);

            procedure Check_Bindings;
            --  After all calls to Note_Binding, check that bindings are
            --  ok (e.g., check consistency among different choices of
            --  one alternative).

         end Case_Bindings;

         procedure Refresh_Binding_Info (Aggr : Node_Id);
         --  The parser records binding-related info in the tree.
         --  The choice nodes that we see here might not be (will never be?)
         --  the original nodes that were produced by the parser. The info
         --  recorded by the parser is missing in that case, so this
         --  procedure recovers it.
         --
         --  There are bugs here. In some cases involving nested aggregates,
         --  the path back to the parser-created nodes is lost. In particular,
         --  we may fail to detect an illegal case like
         --   when (F1 | F2 => (Aa => Natural, Bb => Natural is X)) =>
         --  This should be rejected because it is binding X to both the
         --  F1.Bb and to the F2.Bb subcomponents of the case selector.
         --  It would be nice if the not-specific-to-pattern-matching
         --  aggregate-processing code could remain unaware of the existence
         --  of this binding-related info but perhaps that isn't possible.

         --------------------------
         -- Refresh_Binding_Info --
         --------------------------

         procedure Refresh_Binding_Info (Aggr : Node_Id) is
            Orig_Aggr : constant Node_Id := Original_Node (Aggr);
            Orig_Comp : Node_Id := First (Component_Associations (Orig_Aggr));
         begin
            if Aggr = Orig_Aggr then
               return;
            end if;

            while Present (Orig_Comp) loop
               if Nkind (Orig_Comp) = N_Component_Association
                 and then Binding_Chars (Orig_Comp) /= No_Name
               then
                  if List_Length (Choices (Orig_Comp)) /= 1 then
                     --  Conceivably this could be checked during parsing,
                     --  but checking is easier here.

                     Error_Msg_N
                       ("binding shared by multiple components", Orig_Comp);
                     return;
                  end if;

                  declare
                     Orig_Name : constant Name_Id :=
                       Chars (First (Choices (Orig_Comp)));
                     Comp : Node_Id := First (Component_Associations (Aggr));
                     Matching_Comp : Node_Id := Empty;
                  begin
                     while Present (Comp) loop
                        if Chars (First (Choices (Comp))) = Orig_Name then
                           pragma Assert (No (Matching_Comp));
                           Matching_Comp := Comp;
                        end if;

                        Next (Comp);
                     end loop;

                     pragma Assert (Present (Matching_Comp));

                     Set_Binding_Chars
                       (Matching_Comp,
                        Binding_Chars (Orig_Comp));
                  end;
               end if;

               Next (Orig_Comp);
            end loop;
         end Refresh_Binding_Info;

         ------------------
         -- Parse_Choice --
         ------------------

         function Parse_Choice (Choice : Node_Id;
                                Alt    : Node_Id) return Choice_Range_Info
         is
            Result    : Choice_Range_Info (Is_Others => False);
            Ranges    : Composite_Range_Info renames Result.Ranges;
            Next_Part : Part_Id'Base range 1 .. Part_Id'Last + 1 := 1;

            procedure Traverse_Choice (Expr : Node_Id);
            --  Traverse a legal choice expression, looking for
            --  values/ranges of discrete parts. Call Update_Result
            --  for each.

            procedure Update_Result (Discrete_Range : Discrete_Range_Info);
            --  Initialize first remaining uninitialized element of Ranges.
            --  Also set Next_Part.

            procedure Update_Result_For_Full_Coverage (Comp_Type  : Entity_Id);
            --  For each scalar part of the given component type, call
            --  Update_Result with the full range for that scalar part.
            --  This is used for both box components in aggregates and
            --  for any inactive-variant components that do not appear in
            --  a given aggregate.

            -------------------
            -- Update_Result --
            -------------------

            procedure Update_Result (Discrete_Range : Discrete_Range_Info) is
            begin
               Ranges (Next_Part) := Discrete_Range;
               Next_Part := Next_Part + 1;
            end Update_Result;

            -------------------------------------
            -- Update_Result_For_Full_Coverage --
            -------------------------------------

            procedure Update_Result_For_Full_Coverage (Comp_Type : Entity_Id)
            is
            begin
               for Counter in 1 .. Scalar_Part_Count (Comp_Type) loop
                  Update_Result (Component_Bounds (Next_Part));
               end loop;
            end Update_Result_For_Full_Coverage;

            ---------------------
            -- Traverse_Choice --
            ---------------------

            procedure Traverse_Choice (Expr : Node_Id) is
            begin
               if Nkind (Expr) = N_Qualified_Expression then
                  Traverse_Choice (Expression (Expr));

               elsif Nkind (Expr) = N_Type_Conversion
                  and then not Comes_From_Source (Expr)
               then
                  if Expr /= Original_Node (Expr) then
                     Traverse_Choice (Original_Node (Expr));
                  else
                     Traverse_Choice (Expression (Expr));
                  end if;

               elsif Nkind (Expr) = N_Aggregate then
                  if Is_Record_Type (Etype (Expr)) then
                     Refresh_Binding_Info (Aggr => Expr);

                     declare
                        Comp_Assoc : Node_Id :=
                          First (Component_Associations (Expr));
                        --  Aggregate has been normalized (components in
                        --  order, only one component per choice, etc.).

                        Comp_From_Type : Node_Id :=
                          First_Component_Or_Discriminant
                            (Base_Type (Etype (Expr)));

                        Saved_Next_Part : constant Part_Id := Next_Part;
                     begin
                        while Present (Comp_Assoc) loop
                           pragma Assert
                             (List_Length (Choices (Comp_Assoc)) = 1);

                           declare
                              Comp : constant Node_Id :=
                                Entity (First (Choices (Comp_Assoc)));
                              Comp_Seen : Boolean := False;
                           begin
                              loop
                                 if Original_Record_Component (Comp) =
                                   Original_Record_Component (Comp_From_Type)
                                 then
                                    Comp_Seen := True;
                                 else
                                    --  We have an aggregate of a type that
                                    --  has a variant part (or has a
                                    --  subcomponent type that has a variant
                                    --  part) and we have to deal with a
                                    --  component that is present in the type
                                    --  but not in the aggregate (because the
                                    --  component is in an inactive variant).
                                    --
                                    Update_Result_For_Full_Coverage
                                      (Comp_Type => Etype (Comp_From_Type));
                                 end if;

                                 Comp_From_Type :=
                                   Next_Component_Or_Discriminant
                                     (Comp_From_Type);

                                 exit when Comp_Seen;
                              end loop;
                           end;

                           declare
                              Comp_Type : constant Entity_Id :=
                                Etype (First (Choices (Comp_Assoc)));
                           begin
                              if Box_Value_Required (Comp_Type) then
                                 --  This component is not allowed to
                                 --  influence which alternative is
                                 --  chosen; case choice must be box.
                                 --
                                 --  For example, component might be
                                 --  of a real type or of an access type
                                 --  or of a non-static discrete subtype.
                                 if not Box_Present (Comp_Assoc) then
                                    Error_Msg_N
                                      ("Non-box case choice component value" &
                                         " of unsupported type/subtype",
                                       Expression (Comp_Assoc));
                                 end if;
                              elsif Box_Present (Comp_Assoc) then
                                 --  Box matches all values
                                 Update_Result_For_Full_Coverage
                                   (Etype (First (Choices (Comp_Assoc))));
                              else
                                 Traverse_Choice (Expression (Comp_Assoc));
                              end if;
                           end;

                           if Binding_Chars (Comp_Assoc) /= No_Name
                           then
                              Case_Bindings.Note_Binding
                                (Comp_Assoc => Comp_Assoc,
                                 Choice     => Choice,
                                 Alt        => Alt);
                           end if;

                           Next (Comp_Assoc);
                        end loop;

                        while Present (Comp_From_Type) loop
                           --  Deal with any trailing inactive-variant
                           --  components.
                           --
                           --  See earlier commment about calling
                           --  Update_Result_For_Full_Coverage for such
                           --  components.

                           Update_Result_For_Full_Coverage
                             (Comp_Type => Etype (Comp_From_Type));

                           Comp_From_Type :=
                             Next_Component_Or_Discriminant (Comp_From_Type);
                        end loop;

                        declare
                           Expr_Type : Entity_Id := Etype (Expr);
                        begin
                           if Has_Discriminants (Expr_Type) then
                              --  Avoid nonstatic choice expr types,
                              --  for which Scalar_Part_Count returns 0.
                              Expr_Type := Base_Type (Expr_Type);
                           end if;

                           pragma Assert
                             (Nat (Next_Part - Saved_Next_Part)
                               = Scalar_Part_Count (Expr_Type));
                        end;
                     end;
                  elsif Is_Array_Type (Etype (Expr)) then
                     if Is_Non_Empty_List (Component_Associations (Expr)) then
                        Error_Msg_N
                          ("non-positional array aggregate as/within case "
                           & "choice not implemented", Expr);
                     end if;

                     if not Unconstrained_Array_Case
                        and then List_Length (Expressions (Expr))
                           /= Nat (Part_Id'Last)
                     then
                        Error_Msg_Uint_1 := UI_From_Int
                          (List_Length (Expressions (Expr)));
                        Error_Msg_Uint_2 := UI_From_Int (Int (Part_Id'Last));
                        Error_Msg_N
                          ("array aggregate length ^ does not match length " &
                           "of statically constrained case selector ^", Expr);
                        return;
                     end if;

                     declare
                        Subexpr : Node_Id := First (Expressions (Expr));
                     begin
                        while Present (Subexpr) loop
                           Traverse_Choice (Subexpr);
                           Next (Subexpr);
                        end loop;
                     end;
                  else
                     raise Program_Error;
                  end if;
               elsif Nkind (Expr) = N_String_Literal then
                  if not Is_Array_Type (Etype (Expr)) then
                     Error_Msg_N
                       ("User-defined string literal not allowed as/within"
                        & "case choice", Expr);
                  else
                     declare
                        Char_Type : constant Entity_Id :=
                          Root_Type (Component_Type (Etype (Expr)));

                        --  If the component type is not a standard character
                        --  type then this string lit should have already been
                        --  transformed into an aggregate in
                        --  Resolve_String_Literal.
                        --
                        pragma Assert (Is_Standard_Character_Type (Char_Type));

                        Str      : constant String_Id := Strval (Expr);
                        Strlen   : constant Nat       := String_Length (Str);
                        Char_Val : Uint;
                     begin
                        if not Unconstrained_Array_Case
                           and then Strlen /= Nat (Part_Id'Last)
                        then
                           Error_Msg_Uint_1 := UI_From_Int (Strlen);
                           Error_Msg_Uint_2 := UI_From_Int
                             (Int (Part_Id'Last));
                           Error_Msg_N
                             ("String literal length ^ does not match length" &
                              " of statically constrained case selector ^",
                              Expr);
                           return;
                        end if;

                        for Idx in 1 .. Strlen loop
                           Char_Val :=
                             UI_From_CC (Get_String_Char (Str, Idx));
                           Update_Result ((Low | High => Char_Val));
                        end loop;
                     end;
                  end if;
               elsif Is_Discrete_Type (Etype (Expr)) then
                  if Nkind (Expr) in N_Has_Entity
                    and then Present (Entity (Expr))
                    and then Is_Type (Entity (Expr))
                  then
                     declare
                        Low  : constant Node_Id :=
                          Type_Low_Bound (Entity (Expr));
                        High : constant Node_Id :=
                          Type_High_Bound (Entity (Expr));
                     begin
                        Update_Result ((Low  => Expr_Value (Low),
                                        High => Expr_Value (High)));
                     end;
                  else
                     pragma Assert (Compile_Time_Known_Value (Expr));
                     Update_Result ((Low | High => Expr_Value (Expr)));
                  end if;
               elsif Nkind (Expr) in N_Has_Entity
                 and then Present (Entity (Expr))
                 and then Ekind (Entity (Expr)) = E_Constant
               then
                  Traverse_Choice (Expression (Parent (Entity (Expr))));
               elsif Nkind (Original_Node (Expr))
                       in N_Aggregate | N_String_Literal
               then
                  Traverse_Choice (Original_Node (Expr));
               else
                  Error_Msg_N
                    ("non-aggregate case choice (or subexpression thereof)"
                     & " that is not of a discrete type not implemented",
                     Expr);
               end if;
            end Traverse_Choice;

         --  Start of processing for Parse_Choice

         begin
            if Nkind (Choice) = N_Others_Choice then
               return (Is_Others => True);
            end if;

            if Unconstrained_Array_Case then
               --  Treat length like a discriminant
               Update_Result ((Low | High =>
                                 UI_From_Int (Array_Choice_Length (Choice))));
            end if;

            Traverse_Choice (Choice);

            if Unconstrained_Array_Case then
               --  This is somewhat tricky. Suppose we are casing on String,
               --  the longest choice in the case statement is length 10, and
               --  the choice we are looking at now is of length 6. We fill
               --  in the trailing 4 slots here.
               while Next_Part <= Part_Id'Last loop
                  Update_Result_For_Full_Coverage
                    (Comp_Type => Component_Type (Case_Expr_Type));
               end loop;
            end if;

            --  Avoid returning uninitialized garbage in error case
            if Next_Part /= Part_Id'Last + 1 then
               pragma Assert (Serious_Errors_Detected > 0);
               Result.Ranges := (others => (Low => Uint_1, High => Uint_0));
            end if;

            return Result;
         end Parse_Choice;

         package body Case_Bindings is

            type Binding is record
               Comp_Assoc : Node_Id;
               Choice     : Node_Id;
               Alt        : Node_Id;
            end record;

            type Binding_Index is new Natural;

            package Case_Bindings_Table is new Table.Table
              (Table_Component_Type => Binding,
               Table_Index_Type     => Binding_Index,
               Table_Low_Bound      => 1,
               Table_Initial        => 16,
               Table_Increment      => 64,
               Table_Name           => "Composite_Case_Ops.Case_Bindings");

            ------------------
            -- Note_Binding --
            ------------------

            procedure Note_Binding
              (Comp_Assoc : Node_Id;
               Choice     : Node_Id;
               Alt        : Node_Id)
            is
            begin
               Case_Bindings_Table.Append
                 ((Comp_Assoc => Comp_Assoc,
                   Choice     => Choice,
                   Alt        => Alt));
            end Note_Binding;

            --------------------
            -- Check_Bindings --
            --------------------

            procedure Check_Bindings
            is
               use Case_Bindings_Table;

               function Binding_Subtype (Idx : Binding_Index;
                                         Tab : Table_Type)
                 return Entity_Id is
                 (Etype (Nlists.First (Choices (Tab (Idx).Comp_Assoc))));

               procedure Declare_Binding_Objects
                  (Alt_Start             : Binding_Index;
                   Alt                   : Node_Id;
                   First_Choice_Bindings : Natural;
                   Tab                   : Table_Type);
               --  Declare the binding objects for a given alternative

               ------------------------------
               --  Declare_Binding_Objects --
               ------------------------------

               procedure Declare_Binding_Objects
                  (Alt_Start             : Binding_Index;
                   Alt                   : Node_Id;
                   First_Choice_Bindings : Natural;
                   Tab                   : Table_Type)
               is
                  Loc : constant Source_Ptr := Sloc (Alt);
                  Declarations : constant List_Id := New_List;
                  Decl         : Node_Id;
                  Obj_Type     : Entity_Id;
                  Def_Id       : Entity_Id;
               begin
                  for FC_Idx in Alt_Start ..
                    Alt_Start + Binding_Index (First_Choice_Bindings - 1)
                  loop
                     Obj_Type := Binding_Subtype (FC_Idx, Tab);
                     Def_Id := Make_Defining_Identifier
                                 (Loc,
                                  Binding_Chars (Tab (FC_Idx).Comp_Assoc));

                     --  Either make a copy or rename the original. At a
                     --  minimum, we do not want a copy if it would need
                     --  finalization. Copies may also introduce problems
                     --  if default init can have side effects (although we
                     --  could suppress such default initialization).
                     --  We have to make a copy in any cases where
                     --  Unrestricted_Access doesn't work.
                     --
                     --  This is where the copy-or-rename decision is made.
                     --  In many cases either way would work and so we have
                     --  some flexibility here.

                     if not Is_By_Copy_Type (Obj_Type) then
                        --  Generate
                        --     type Ref
                        --       is access constant Obj_Type;
                        --     Ptr : Ref := <some bogus value>;
                        --     Obj : Obj_Type renames Ptr.all;
                                       --
                        --  Initialization of Ptr will be generated later
                        --  during expansion.

                        declare
                           Ptr_Type : constant Entity_Id :=
                             Make_Temporary (Loc, 'P');

                           Ptr_Type_Def : constant Node_Id :=
                             Make_Access_To_Object_Definition (Loc,
                               All_Present => True,
                               Subtype_Indication =>
                                 New_Occurrence_Of (Obj_Type, Loc));

                           Ptr_Type_Decl : constant Node_Id :=
                             Make_Full_Type_Declaration (Loc,
                               Ptr_Type,
                               Type_Definition => Ptr_Type_Def);

                           Ptr_Obj : constant Entity_Id :=
                             Make_Temporary (Loc, 'T');

                           --  We will generate initialization code for this
                           --  object later (during expansion) but in the
                           --  meantime we don't want the dereference that
                           --  is generated a few lines below here to be
                           --  transformed into a Raise_C_E. To prevent this,
                           --  we provide a bogus initial value here; this
                           --  initial value will be removed later during
                           --  expansion.

                           Ptr_Obj_Decl : constant Node_Id :=
                             Make_Object_Declaration
                               (Loc, Ptr_Obj,
                                Object_Definition =>
                                  New_Occurrence_Of (Ptr_Type, Loc),
                                Expression =>
                                  Unchecked_Convert_To
                                    (Ptr_Type,
                                     Make_Integer_Literal (Loc, 5432)));
                        begin
                           Mutate_Ekind (Ptr_Type, E_Access_Type);

                           --  in effect, Storage_Size => 0
                           Set_No_Pool_Assigned (Ptr_Type);

                           Set_Is_Access_Constant (Ptr_Type);

                           --  We could set Ptr_Type'Alignment here if that
                           --  ever turns out to be needed for renaming a
                           --  misaligned subcomponent.

                           Mutate_Ekind (Ptr_Obj, E_Variable);
                           Set_Etype (Ptr_Obj, Ptr_Type);

                           Decl :=
                             Make_Object_Renaming_Declaration
                               (Loc, Def_Id,
                                Subtype_Mark =>
                                  New_Occurrence_Of (Obj_Type, Loc),
                                Name =>
                                  Make_Explicit_Dereference
                                    (Loc, New_Occurrence_Of (Ptr_Obj, Loc)));

                           Append_To (Declarations, Ptr_Type_Decl);
                           Append_To (Declarations, Ptr_Obj_Decl);
                        end;
                     else
                        Decl := Make_Object_Declaration
                          (Sloc => Loc,
                           Defining_Identifier => Def_Id,
                           Object_Definition =>
                              New_Occurrence_Of (Obj_Type, Loc));
                     end if;
                     Append_To (Declarations, Decl);
                  end loop;

                  declare
                     Old_Statements : constant List_Id := Statements (Alt);
                     New_Statements : constant List_Id := New_List;

                     Block_Statement : constant Node_Id :=
                       Make_Block_Statement (Sloc => Loc,
                         Declarations => Declarations,
                         Handled_Statement_Sequence =>
                           Make_Handled_Sequence_Of_Statements
                             (Loc, Old_Statements),
                         Has_Created_Identifier => True);
                  begin
                     Append_To (New_Statements, Block_Statement);
                     Set_Statements (Alt, New_Statements);
                  end;
               end Declare_Binding_Objects;
            begin
               if Last = 0 then
                  --  no bindings to check
                  return;
               end if;

               declare
                  Tab : Table_Type
                          renames Case_Bindings_Table.Table (1 .. Last);

                  function Same_Id (Idx1, Idx2 : Binding_Index)
                    return Boolean is (
                    Binding_Chars (Tab (Idx1).Comp_Assoc) =
                    Binding_Chars (Tab (Idx2).Comp_Assoc));
               begin
                  --  Verify that elements with given choice or alt value
                  --  are contiguous, and that elements with equal
                  --  choice values have same alt value.

                  for Idx1 in 2 .. Tab'Last loop
                     if Tab (Idx1 - 1).Choice /= Tab (Idx1).Choice then
                        pragma Assert
                          (for all Idx2 in Idx1 + 1 .. Tab'Last =>
                             Tab (Idx2).Choice /= Tab (Idx1 - 1).Choice);
                     else
                        pragma Assert (Tab (Idx1 - 1).Alt = Tab (Idx1).Alt);
                     end if;
                     if Tab (Idx1 - 1).Alt /= Tab (Idx1).Alt then
                        pragma Assert
                          (for all Idx2 in Idx1 + 1 .. Tab'Last =>
                             Tab (Idx2).Alt /= Tab (Idx1 - 1).Alt);
                     end if;
                  end loop;

                  --  Check for user errors:
                  --  1) Two choices for a given alternative shall define the
                  --     same set of names. Can't have
                  --        when (<X>, 0) | (0, <Y>) =>
                  --  2) A choice shall not define a name twice. Can't have
                  --        when (A => <X>, B => <X>, C => 0) =>
                  --  3) Two definitions of a name within one alternative
                  --     shall have statically matching component subtypes.
                  --     Can't have
                  --        type R is record Int : Integer;
                  --                         Nat : Natural; end record;
                  --        case R'(...) is
                  --          when (<X>, 1) | (1, <X>) =>
                  --  4) A given binding shall match only one value.
                  --     Can't have
                  --         (Fld1 | Fld2 => (Fld => <X>))
                  --     For now, this is enforced *very* conservatively
                  --     with respect to arrays - a binding cannot match
                  --     any part of an array. This is temporary.

                  for Idx1 in Tab'Range loop
                     if Idx1 = 1
                       or else Tab (Idx1 - 1).Alt /= Tab (Idx1).Alt
                     then
                        --  Process one alternative
                        declare
                           Alt_Start : constant Binding_Index := Idx1;
                           Alt : constant Node_Id := Tab (Alt_Start).Alt;

                           First_Choice : constant Node_Id :=
                             Nlists.First (Discrete_Choices (Alt));
                           First_Choice_Bindings : Natural := 0;
                        begin
                           --  Check for duplicates within one choice,
                           --  and for choices with no bindings.

                           if First_Choice /= Tab (Alt_Start).Choice then
                              Error_Msg_N ("binding(s) missing for choice",
                                           First_Choice);
                              return;
                           end if;

                           declare
                              Current_Choice : Node_Id := First_Choice;
                              Choice_Start : Binding_Index := Alt_Start;
                           begin
                              for Idx2 in Alt_Start .. Tab'Last loop
                                 exit when Tab (Idx2).Alt /= Alt;
                                 if Tab (Idx2).Choice = Current_Choice then
                                    for Idx3 in Choice_Start .. Idx2 - 1 loop
                                       if Same_Id (Idx2, Idx3)
                                       then
                                          Error_Msg_N
                                            ("duplicate binding in choice",
                                             Current_Choice);
                                          return;
                                       end if;
                                    end loop;
                                 else
                                    Next (Current_Choice);
                                    pragma Assert (Present (Current_Choice));
                                    Choice_Start := Idx2;

                                    if Tab (Idx2).Choice /= Current_Choice
                                    then
                                       Error_Msg_N
                                         ("binding(s) missing for choice",
                                          Current_Choice);
                                       return;
                                    end if;
                                 end if;
                              end loop;

                              --  If we made it through all the bindings
                              --  for this alternative but didn't make it
                              --  to the last choice, then bindings are
                              --  missing for all remaining choices.
                              --  We only complain about the first one.

                              if Present (Next (Current_Choice)) then
                                 Error_Msg_N
                                   ("binding(s) missing for choice",
                                     Next (Current_Choice));
                                 return;
                              end if;
                           end;

                           --  Count bindings for first choice of alternative

                           for FC_Idx in Alt_Start .. Tab'Last loop
                              exit when Tab (FC_Idx).Choice /= First_Choice;
                              First_Choice_Bindings :=
                                First_Choice_Bindings + 1;
                           end loop;

                           declare
                              Current_Choice : Node_Id := First_Choice;
                              Current_Choice_Bindings : Natural := 0;
                           begin
                              for Idx2 in Alt_Start .. Tab'Last loop
                                 exit when Tab (Idx2).Alt /= Alt;

                                 --  If starting a new choice

                                 if Tab (Idx2).Choice /= Current_Choice then

                                    --  Check count for choice just finished

                                    if Current_Choice_Bindings
                                      /= First_Choice_Bindings
                                    then
                                       Error_Msg_N
                                         ("subsequent choice has different"
                                          & " number of bindings than first"
                                          & " choice", Current_Choice);
                                    end if;

                                    Current_Choice := Tab (Idx2).Choice;
                                    Current_Choice_Bindings := 1;

                                    --  Remember that Alt has both one or more
                                    --  bindings and two or more choices; we'll
                                    --  need to know this during expansion.

                                    Set_Multidefined_Bindings (Alt, True);
                                 else
                                    Current_Choice_Bindings :=
                                      Current_Choice_Bindings + 1;
                                 end if;

                                 --  Check that first choice has binding with
                                 --  matching name; check subtype consistency.

                                 declare
                                    Found : Boolean := False;
                                 begin
                                    for FC_Idx in
                                      Alt_Start ..
                                      Alt_Start + Binding_Index
                                                    (First_Choice_Bindings - 1)
                                    loop
                                       if Same_Id (Idx2, FC_Idx) then
                                          if not Subtypes_Statically_Match
                                            (Binding_Subtype (Idx2, Tab),
                                             Binding_Subtype (FC_Idx, Tab))
                                          then
                                             Error_Msg_N
                                               ("subtype of binding in "
                                                & "subsequent choice does not "
                                                & "match that in first choice",
                                                Tab (Idx2).Comp_Assoc);
                                          end if;
                                          Found := True;
                                          exit;
                                       end if;
                                    end loop;

                                    if not Found then
                                       Error_Msg_N
                                         ("binding defined in subsequent "
                                          & "choice not defined in first "
                                          & "choice", Current_Choice);
                                    end if;
                                 end;

                                 --  Check for illegal repeated binding
                                 --  via an enclosing aggregate, as in
                                 --  (F1 | F2 => (F3 => Natural is X,
                                 --               F4 => Natural))
                                 --  where the inner aggregate would be ok.

                                 declare
                                    Rover : Node_Id := Tab (Idx2).Comp_Assoc;
                                 begin
                                    while Rover /= Tab (Idx2).Choice loop
                                       Rover :=
                                         (if Is_List_Member (Rover) then
                                            Parent (List_Containing (Rover))
                                          else Parent (Rover));
                                       pragma Assert (Present (Rover));
                                       if Nkind (Rover)
                                         = N_Component_Association
                                         and then List_Length (Choices (Rover))
                                         > 1
                                       then
                                          Error_Msg_N
                                            ("binding shared by multiple "
                                                & "enclosing components",
                                             Tab (Idx2).Comp_Assoc);
                                       end if;
                                    end loop;
                                 end;
                              end loop;
                           end;

                           --  Construct the (unanalyzed) declarations for
                           --  the current alternative. Then analyze them.

                           if First_Choice_Bindings > 0 then
                              Declare_Binding_Objects
                                (Alt_Start             => Alt_Start,
                                 Alt                   => Alt,
                                 First_Choice_Bindings =>
                                   First_Choice_Bindings,
                                 Tab                   => Tab);
                           end if;
                        end;
                     end if;
                  end loop;
               end;
            end Check_Bindings;
         end Case_Bindings;

         function Choice_Bounds_Info return Choices_Range_Info;
         --  Returns mapping from any given Choice_Id value to that choice's
         --  component-to-range map.

         ------------------------
         -- Choice_Bounds_Info --
         ------------------------

         function Choice_Bounds_Info return Choices_Range_Info is
            Result : Choices_Range_Info;
            Alt    : Node_Id := First (Alternatives (Case_Statement));
            C_Id   : Choice_Id := 1;
         begin
            while Present (Alt) loop
               declare
                  Choice : Node_Id := First (Discrete_Choices (Alt));
               begin
                  while Present (Choice) loop
                     Result (C_Id) := Parse_Choice (Choice, Alt => Alt);

                     Next (Choice);
                     if C_Id /= Choice_Id'Last then
                        C_Id := C_Id + 1;
                     end if;
                  end loop;
               end;
               Next (Alt);
            end loop;

            pragma Assert (C_Id = Choice_Id'Last);

            --  No more calls to Note_Binding, so time for checks.
            Case_Bindings.Check_Bindings;

            return Result;
         end Choice_Bounds_Info;

         Choices_Bounds : constant Choices_Range_Info := Choice_Bounds_Info;

         package body Value_Sets is
            use GNAT;

            function Hash (Key : Uint) return Bucket_Range_Type is
              (Bucket_Range_Type
                 (UI_To_Int (Key mod (Uint_2 ** Uint_31))));

            package Uint_Sets is new GNAT.Sets.Membership_Sets
              (Uint, "=", Hash);

            type Representative_Values_Array is
              array (Part_Id) of Uint_Sets.Membership_Set;

            function Representative_Values_Init
              return Representative_Values_Array;
            --  Select the representative values for each Part_Id value.
            --  This function is called exactly once, immediately after it
            --  is declared.

            --------------------------------
            -- Representative_Values_Init --
            --------------------------------

            function Representative_Values_Init
              return Representative_Values_Array
            is
               --  For each range of each choice (as well as the range for the
               --  component subtype, which is handled in the first loop),
               --  insert the low bound of the range and the successor of
               --  the high bound into the corresponding R_V element.
               --
               --  The idea we are trying to capture here is somewhat tricky.
               --  Given an arbitrary point P1 in the Cartesian product
               --  of the Component_Bounds sets, we want to be able
               --  to map that to a point P2 in the (smaller) Cartesian product
               --  of the Representative_Values sets that has the property
               --  that for every choice of the case statement, P1 matches
               --  the choice if and only if P2 also matches. Given that,
               --  we can implement the overlapping/containment/etc. rules
               --  safely by just looking at (using brute force enumeration)
               --  the (smaller) Cartesian product of the R_V sets.
               --  We are never going to actually perform this point-to-point
               --  mapping - just the fact that it exists is enough to ensure
               --  we can safely look at just the R_V sets.
               --
               --  The desired mapping can be implemented by mapping a point
               --  P1 to a point P2 by reducing each of P1's coordinates down
               --  to the largest element of the corresponding R_V set that is
               --  less than or equal to the original coordinate value (such
               --  an element Y will always exist because the R_V set for a
               --  given component always includes the low bound of the
               --  component subtype). It then suffices to show that every
               --  choice in the case statement yields the same Boolean result
               --  for P1 as for P2.
               --
               --  Suppose the contrary. Then there is some particular
               --  coordinate position X (i.e., a Part_Id value) and some
               --  choice C where exactly one of P1(X) and P2(X) belongs to
               --  the (contiguous) range associated with C(X); call that
               --  range L .. H. We know that P2(X) <= P1(X) because the
               --  mapping never increases coordinate values. Consider three
               --  cases: P1(X) lies within the L .. H range, or it is greater
               --  than H, or it is lower than L.
               --  The third case is impossible because reducing a value that
               --  is less than L can only produce another such value,
               --  violating the "exactly one" assumption. The second
               --  case is impossible because L belongs to the corresponding
               --  R_V set, so P2(X) >= L and both values belong to the
               --  range, again violating the "exactly one" assumption.
               --  Finally, the third case is impossible because H+1 belongs
               --  to the corresponding R_V set, so P2(X) > H, so neither
               --  value belongs to the range, again violating the "exactly
               --  one" assumption. So our initial supposition was wrong. QED.

               use Uint_Sets;

               Result : constant Representative_Values_Array :=
                 (others => Uint_Sets.Create (Initial_Size => 32));

               procedure Insert_Representative (Value : Uint; P : Part_Id);
               --  Insert the given Value into the representative values set
               --  for the given component if it belongs to the component's
               --  subtype. Otherwise, do nothing.

               ---------------------------
               -- Insert_Representative --
               ---------------------------

               procedure Insert_Representative (Value : Uint; P : Part_Id) is
               begin
                  if Value >= Component_Bounds (P).Low and
                    Value <= Component_Bounds (P).High
                  then
                     Insert (Result (P), Value);
                  end if;
               end Insert_Representative;

            begin
               for P in Part_Id loop
                  Insert_Representative (Component_Bounds (P).Low, P);
               end loop;

               if Simplified_Composite_Coverage_Rules then
                  --  Omit other representative values to avoid capacity
                  --  problems building data structures only used in
                  --  compile-time checks that will not be performed.
                  return Result;
               end if;

               for C of Choices_Bounds loop
                  if not C.Is_Others then
                     for P in Part_Id loop
                        if C.Ranges (P).Low <= C.Ranges (P).High then
                           Insert_Representative (C.Ranges (P).Low, P);
                           Insert_Representative (C.Ranges (P).High + 1, P);
                        end if;
                     end loop;
                  end if;
               end loop;
               return Result;
            end Representative_Values_Init;

            Representative_Values : constant Representative_Values_Array :=
              Representative_Values_Init;
            --  We want to avoid looking at every point in the Cartesian
            --  product of all component values. Instead we select, for each
            --  component, a set of representative values and then look only
            --  at the Cartesian product of those sets. A single value can
            --  safely represent a larger enclosing interval if every choice
            --  for that component either completely includes or completely
            --  excludes the interval. The elements of this array will be
            --  populated by a call to Initialize_Representative_Values and
            --  will remain constant after that.

            type Value_Index_Base is new Natural;

            function Value_Index_Count return Value_Index_Base;
            --  Returns the product of the sizes of the Representative_Values
            --  sets (i.e., the size of the Cartesian product of the sets).
            --  May return zero if one of the sets is empty.
            --  This function is called exactly once, immediately after it
            --  is declared.

            -----------------------
            -- Value_Index_Count --
            -----------------------

            function Value_Index_Count return Value_Index_Base is
               Result : Value_Index_Base := 1;
            begin
               for Set of Representative_Values loop
                  Result := Result * Value_Index_Base (Uint_Sets.Size (Set));
               end loop;
               return Result;
            exception
               when Constraint_Error =>
                  Error_Msg_N
                    ("Capacity exceeded in compiling case statement with"
                      & " composite selector type", Case_Statement);
                  raise;
            end Value_Index_Count;

            Max_Value_Index : constant Value_Index_Base := Value_Index_Count;

            subtype Value_Index is Value_Index_Base range 1 .. Max_Value_Index;
            type Value_Index_Set is array (Value_Index) of Boolean;

            package Value_Index_Set_Table is new Table.Table
              (Table_Component_Type => Value_Index_Set,
               Table_Index_Type     => Value_Set,
               Table_Low_Bound      => 1,
               Table_Initial        => 16,
               Table_Increment      => 100,
               Table_Name           => "Composite_Case_Ops.Value_Sets");
            --  A nonzero Value_Set value is an index into this table.

            function Indexed (Index : Value_Set) return Value_Index_Set
              is (Value_Index_Set_Table.Table.all (Index));

            function Allocate_Table_Element (Initial_Value : Value_Index_Set)
              return Value_Set;
            --  Allocate and initialize a new table element; return its index.

            ----------------------------
            -- Allocate_Table_Element --
            ----------------------------

            function Allocate_Table_Element (Initial_Value : Value_Index_Set)
              return Value_Set
            is
               use Value_Index_Set_Table;
            begin
               Append (Initial_Value);
               return Last;
            end Allocate_Table_Element;

            procedure Assign_Table_Element (Index : Value_Set;
                                            Value : Value_Index_Set);
            --  Assign specified value to specified table element.

            --------------------------
            -- Assign_Table_Element --
            --------------------------

            procedure Assign_Table_Element (Index : Value_Set;
                                            Value : Value_Index_Set)
            is
            begin
               Value_Index_Set_Table.Table.all (Index) := Value;
            end Assign_Table_Element;

            -------------
            -- Compare --
            -------------

            function Compare (S1, S2 : Value_Set) return Set_Comparison is
            begin
               if S1 = Empty or S2 = Empty then
                  return Disjoint;
               elsif Indexed (S1) = Indexed (S2) then
                  return Equal;
               else
                  declare
                     Intersection : constant Value_Index_Set :=
                       Indexed (S1) and Indexed (S2);
                  begin
                     if (for all Flag of Intersection => not Flag) then
                        return Disjoint;
                     elsif Intersection = Indexed (S1) then
                        return Contained_By;
                     elsif Intersection = Indexed (S2) then
                        return Contains;
                     else
                        return Overlaps;
                     end if;
                  end;
               end if;
            end Compare;

            -------------------------
            -- Complement_Is_Empty --
            -------------------------

            function Complement_Is_Empty (Set : Value_Set) return Boolean
              is (Set /= Empty
                  and then (for all Flag of Indexed (Set) => Flag));

            ---------------------
            -- Free_Value_Sets --
            ---------------------

            procedure Free_Value_Sets is
            begin
               Value_Index_Set_Table.Free;
            end Free_Value_Sets;

            -----------
            -- Union --
            -----------

            procedure Union (Target : in out Value_Set; Source : Value_Set) is
            begin
               if Source /= Empty then
                  if Target = Empty then
                     Target := Allocate_Table_Element (Indexed (Source));
                  else
                     Assign_Table_Element
                       (Target, Indexed (Target) or Indexed (Source));
                  end if;
               end if;
            end Union;

            ------------
            -- Remove --
            ------------

            procedure Remove (Target : in out Value_Set; Source : Value_Set) is
            begin
               if Source /= Empty and Target /= Empty then
                  Assign_Table_Element
                    (Target, Indexed (Target) and not Indexed (Source));
                  if (for all V of Indexed (Target) => not V) then
                     Target := Empty;
                  end if;
               end if;
            end Remove;

            ---------------------
            -- Matching_Values --
            ---------------------

            function Matching_Values
              (Info : Composite_Range_Info) return Value_Set
            is
               Matches    : Value_Index_Set;
               Next_Index : Value_Index := 1;
               Done       : Boolean := False;
               Point      : array (Part_Id) of Uint;

               procedure Test_Point_For_Match;
               --  Point identifies a point in the Cartesian product of the
               --  representative value sets. Record whether that Point
               --  belongs to the product-of-ranges specified by Info.

               --------------------------
               -- Test_Point_For_Match --
               --------------------------

               procedure Test_Point_For_Match is
                  function In_Range (Val : Uint; Rang : Discrete_Range_Info)
                    return Boolean is
                    (Rang.Low <= Val and then Val <= Rang.High);
               begin
                  pragma Assert (not Done);
                  Matches (Next_Index) :=
                    (for all P in Part_Id => In_Range (Point (P), Info (P)));
                  if Next_Index = Matches'Last then
                     Done := True;
                  else
                     Next_Index := Next_Index + 1;
                  end if;
               end Test_Point_For_Match;

               procedure Test_Points (P : Part_Id);
               --  Iterate over the Cartesian product of the representative
               --  value sets, calling Test_Point_For_Match for each point.

               -----------------
               -- Test_Points --
               -----------------

               procedure Test_Points (P : Part_Id) is
                  use Uint_Sets;
                  Iter : Iterator := Iterate (Representative_Values (P));
               begin
                  --  We could traverse here in sorted order, as opposed to
                  --  whatever order the set iterator gives us.
                  --  No need for that as long as every iteration over
                  --  a given representative values set yields the same order.
                  --  Not sorting is more efficient, but it makes it harder to
                  --  interpret a Value_Index_Set bit vector when debugging.

                  while Has_Next (Iter) loop
                     Next (Iter, Point (P));

                     --  If we have finished building up a Point value, then
                     --  test it for matching. Otherwise, recurse to continue
                     --  building up a point value.

                     if P = Part_Id'Last then
                        Test_Point_For_Match;
                     else
                        Test_Points (P + 1);
                     end if;
                  end loop;
               end Test_Points;

            begin
               Test_Points (1);
               if (for all Flag of Matches => not Flag) then
                  return Empty;
               end if;
               return Allocate_Table_Element (Matches);
            end Matching_Values;

         end Value_Sets;

         --------------
         -- Analysis --
         --------------

         function Analysis return Choices_Info is
            Result : Choices_Info;
            Alt    : Node_Id := First (Alternatives (Case_Statement));
            A_Id   : Alternative_Id := 1;
            C_Id   : Choice_Id := 1;
         begin
            while Present (Alt) loop
               declare
                  Choice : Node_Id := First (Discrete_Choices (Alt));
               begin
                  while Present (Choice) loop
                     if Nkind (Choice) = N_Others_Choice then
                        pragma Assert (Choices_Bounds (C_Id).Is_Others);
                        Result (C_Id) :=
                          (Alternative => A_Id,
                           Is_Others   => True);
                     else
                        Result (C_Id) :=
                          (Alternative => A_Id,
                           Is_Others   => False,
                           Matches     => Value_Sets.Matching_Values
                                            (Choices_Bounds (C_Id).Ranges));
                     end if;
                     Next (Choice);
                     if C_Id /= Choice_Id'Last then
                        C_Id := C_Id + 1;
                     end if;
                  end loop;
               end;

               Next (Alt);
               if A_Id /= Alternative_Id'Last then
                  A_Id := A_Id + 1;
               end if;
            end loop;

            pragma Assert (A_Id = Alternative_Id'Last);
            pragma Assert (C_Id = Choice_Id'Last);

            return Result;
         end Analysis;

      end Choice_Analysis;

   end Composite_Case_Ops;

   --------------------------
   -- Expand_Others_Choice --
   --------------------------

   procedure Expand_Others_Choice
     (Case_Table    : Choice_Table_Type;
      Others_Choice : Node_Id;
      Choice_Type   : Entity_Id)
   is
      Loc         : constant Source_Ptr := Sloc (Others_Choice);
      Choice_List : constant List_Id    := New_List;
      Choice      : Node_Id;
      Exp_Lo      : Node_Id;
      Exp_Hi      : Node_Id;
      Hi          : Uint;
      Lo          : Uint;
      Previous_Hi : Uint;

      function Build_Choice (Value1, Value2 : Uint) return Node_Id;
      --  Builds a node representing the missing choices given by Value1 and
      --  Value2. A N_Range node is built if there is more than one literal
      --  value missing. Otherwise a single N_Integer_Literal, N_Identifier
      --  or N_Character_Literal is built depending on what Choice_Type is.

      function Lit_Of (Value : Uint) return Node_Id;
      --  Returns the Node_Id for the enumeration literal corresponding to the
      --  position given by Value within the enumeration type Choice_Type. The
      --  returned value has its Is_Static_Expression flag set to true.

      ------------------
      -- Build_Choice --
      ------------------

      function Build_Choice (Value1, Value2 : Uint) return Node_Id is
         Lit_Node : Node_Id;
         Lo, Hi   : Node_Id;

      begin
         --  If there is only one choice value missing between Value1 and
         --  Value2, build an integer or enumeration literal to represent it.

         if Value1 = Value2 then
            if Is_Integer_Type (Choice_Type) then
               Lit_Node := Make_Integer_Literal (Loc, Value1);
               Set_Etype (Lit_Node, Choice_Type);
               Set_Is_Static_Expression (Lit_Node);
            else
               Lit_Node := Lit_Of (Value1);
            end if;

         --  Otherwise is more that one choice value that is missing between
         --  Value1 and Value2, therefore build a N_Range node of either
         --  integer or enumeration literals.

         else
            if Is_Integer_Type (Choice_Type) then
               Lo := Make_Integer_Literal (Loc, Value1);
               Set_Etype (Lo, Choice_Type);
               Set_Is_Static_Expression (Lo);
               Hi := Make_Integer_Literal (Loc, Value2);
               Set_Etype (Hi, Choice_Type);
               Set_Is_Static_Expression (Hi);
               Lit_Node :=
                 Make_Range (Loc,
                   Low_Bound  => Lo,
                   High_Bound => Hi);

            else
               Lit_Node :=
                 Make_Range (Loc,
                   Low_Bound  => Lit_Of (Value1),
                   High_Bound => Lit_Of (Value2));
            end if;
         end if;

         return Lit_Node;
      end Build_Choice;

      ------------
      -- Lit_Of --
      ------------

      function Lit_Of (Value : Uint) return Node_Id is
         Lit : Entity_Id;

      begin
         --  In the case where the literal is of type Character, there needs
         --  to be some special handling since there is no explicit chain
         --  of literals to search. Instead, a N_Character_Literal node
         --  is created with the appropriate Char_Code and Chars fields.

         if Is_Standard_Character_Type (Choice_Type) then
            Set_Character_Literal_Name (UI_To_CC (Value));
            Lit :=
              Make_Character_Literal (Loc,
                Chars              => Name_Find,
                Char_Literal_Value => Value);
            Set_Etype (Lit, Choice_Type);
            Set_Is_Static_Expression (Lit, True);
            return Lit;

         --  Otherwise, iterate through the literals list of Choice_Type
         --  "Value" number of times until the desired literal is reached
         --  and then return an occurrence of it.

         else
            Lit := First_Literal (Choice_Type);
            for J in 1 .. UI_To_Int (Value) loop
               Next_Literal (Lit);
            end loop;

            return New_Occurrence_Of (Lit, Loc);
         end if;
      end Lit_Of;

   --  Start of processing for Expand_Others_Choice

   begin
      if Case_Table'Last = 0 then

         --  Special case: only an others case is present. The others case
         --  covers the full range of the type.

         if Is_OK_Static_Subtype (Choice_Type) then
            Choice := New_Occurrence_Of (Choice_Type, Loc);
         else
            Choice := New_Occurrence_Of (Base_Type (Choice_Type), Loc);
         end if;

         Set_Others_Discrete_Choices (Others_Choice, New_List (Choice));
         return;
      end if;

      --  Establish the bound values for the choice depending upon whether the
      --  type of the case statement is static or not.

      if Is_OK_Static_Subtype (Choice_Type) then
         Exp_Lo := Type_Low_Bound (Choice_Type);
         Exp_Hi := Type_High_Bound (Choice_Type);
      else
         Exp_Lo := Type_Low_Bound (Base_Type (Choice_Type));
         Exp_Hi := Type_High_Bound (Base_Type (Choice_Type));
      end if;

      Lo := Expr_Value (Case_Table (1).Lo);
      Hi := Expr_Value (Case_Table (1).Hi);
      Previous_Hi := Expr_Value (Case_Table (1).Hi);

      --  Build the node for any missing choices that are smaller than any
      --  explicit choices given in the case.

      if Expr_Value (Exp_Lo) < Lo then
         Append (Build_Choice (Expr_Value (Exp_Lo), Lo - 1), Choice_List);
      end if;

      --  Build the nodes representing any missing choices that lie between
      --  the explicit ones given in the case.

      for J in 2 .. Case_Table'Last loop
         Lo := Expr_Value (Case_Table (J).Lo);
         Hi := Expr_Value (Case_Table (J).Hi);

         if Lo /= (Previous_Hi + 1) then
            Append_To (Choice_List, Build_Choice (Previous_Hi + 1, Lo - 1));
         end if;

         Previous_Hi := Hi;
      end loop;

      --  Build the node for any missing choices that are greater than any
      --  explicit choices given in the case.

      if Expr_Value (Exp_Hi) > Hi then
         Append (Build_Choice (Hi + 1, Expr_Value (Exp_Hi)), Choice_List);
      end if;

      Set_Others_Discrete_Choices (Others_Choice, Choice_List);

      --  Warn on null others list if warning option set

      if Warn_On_Redundant_Constructs
        and then Comes_From_Source (Others_Choice)
        and then Is_Empty_List (Choice_List)
      then
         Error_Msg_N ("?r?OTHERS choice is redundant", Others_Choice);
         Error_Msg_N ("\?r?previous choices cover all values", Others_Choice);
      end if;
   end Expand_Others_Choice;

   -----------
   -- No_OP --
   -----------

   procedure No_OP (C : Node_Id) is
   begin
      if Nkind (C) = N_Range and then Warn_On_Redundant_Constructs then
         Error_Msg_N ("choice is an empty range?r?", C);
      end if;
   end No_OP;

   -----------------------------
   -- Generic_Analyze_Choices --
   -----------------------------

   package body Generic_Analyze_Choices is

      --  The following type is used to gather the entries for the choice
      --  table, so that we can then allocate the right length.

      type Link;
      type Link_Ptr is access all Link;

      type Link is record
         Val : Choice_Bounds;
         Nxt : Link_Ptr;
      end record;

      ---------------------
      -- Analyze_Choices --
      ---------------------

      procedure Analyze_Choices
        (Alternatives : List_Id;
         Subtyp       : Entity_Id)
      is
         Choice_Type : constant Entity_Id := Base_Type (Subtyp);
         --  The actual type against which the discrete choices are resolved.
         --  Note that this type is always the base type not the subtype of the
         --  ruling expression, index or discriminant.

         Expected_Type : Entity_Id;
         --  The expected type of each choice. Equal to Choice_Type, except if
         --  the expression is universal, in which case the choices can be of
         --  any integer type.

         Alt : Node_Id;
         --  A case statement alternative or a variant in a record type
         --  declaration.

         Choice : Node_Id;
         Kind   : Node_Kind;
         --  The node kind of the current Choice

      begin
         --  Set Expected type (= choice type except for universal integer,
         --  where we accept any integer type as a choice).

         if Choice_Type = Universal_Integer then
            Expected_Type := Any_Integer;
         else
            Expected_Type := Choice_Type;
         end if;

         --  Now loop through the case alternatives or record variants

         Alt := First (Alternatives);
         while Present (Alt) loop

            --  If pragma, just analyze it

            if Nkind (Alt) = N_Pragma then
               Analyze (Alt);

            --  Otherwise we have an alternative. In most cases the semantic
            --  processing leaves the list of choices unchanged

            --  Check each choice against its base type

            else
               Choice := First (Discrete_Choices (Alt));
               while Present (Choice) loop
                  Analyze (Choice);
                  Kind := Nkind (Choice);

                  --  Choice is a Range

                  if Kind = N_Range
                    or else (Kind = N_Attribute_Reference
                              and then Attribute_Name (Choice) = Name_Range)
                  then
                     Resolve (Choice, Expected_Type);

                  --  Choice is a subtype name, nothing further to do now

                  elsif Is_Entity_Name (Choice)
                    and then Is_Type (Entity (Choice))
                  then
                     null;

                  --  Choice is a subtype indication

                  elsif Kind = N_Subtype_Indication then
                     Resolve_Discrete_Subtype_Indication
                       (Choice, Expected_Type);

                  --  Others choice, no analysis needed

                  elsif Kind = N_Others_Choice then
                     null;

                  --  Only other possibility is an expression

                  else
                     Resolve (Choice, Expected_Type);
                  end if;

                  --  Move to next choice

                  Next (Choice);
               end loop;

               Process_Associated_Node (Alt);
            end if;

            Next (Alt);
         end loop;
      end Analyze_Choices;

   end Generic_Analyze_Choices;

   ---------------------------
   -- Generic_Check_Choices --
   ---------------------------

   package body Generic_Check_Choices is

      --  The following type is used to gather the entries for the choice
      --  table, so that we can then allocate the right length.

      type Link;
      type Link_Ptr is access all Link;

      type Link is record
         Val : Choice_Bounds;
         Nxt : Link_Ptr;
      end record;

      procedure Free is new Ada.Unchecked_Deallocation (Link, Link_Ptr);

      -------------------
      -- Check_Choices --
      -------------------

      procedure Check_Choices
        (N              : Node_Id;
         Alternatives   : List_Id;
         Subtyp         : Entity_Id;
         Others_Present : out Boolean)
      is
         E : Entity_Id;

         Raises_CE : Boolean;
         --  Set True if one of the bounds of a choice raises CE

         Enode : Node_Id;
         --  This is where we post error messages for bounds out of range

         Choice_List : Link_Ptr := null;
         --  Gather list of choices

         Num_Choices : Nat := 0;
         --  Number of entries in Choice_List

         Choice_Type : constant Entity_Id := Base_Type (Subtyp);
         --  The actual type against which the discrete choices are resolved.
         --  Note that this type is always the base type not the subtype of the
         --  ruling expression, index or discriminant.

         Bounds_Type : Entity_Id;
         --  The type from which are derived the bounds of the values covered
         --  by the discrete choices (see 3.8.1 (4)). If a discrete choice
         --  specifies a value outside of these bounds we have an error.

         Bounds_Lo : Uint;
         Bounds_Hi : Uint;
         --  The actual bounds of the above type

         Expected_Type : Entity_Id;
         --  The expected type of each choice. Equal to Choice_Type, except if
         --  the expression is universal, in which case the choices can be of
         --  any integer type.

         Alt : Node_Id;
         --  A case statement alternative or a variant in a record type
         --  declaration.

         Choice : Node_Id;
         Kind   : Node_Kind;
         --  The node kind of the current Choice

         Others_Choice : Node_Id := Empty;
         --  Remember others choice if it is present (empty otherwise)

         procedure Check (Choice : Node_Id; Lo, Hi : Node_Id);
         --  Checks the validity of the bounds of a choice. When the bounds
         --  are static and no error occurred the bounds are collected for
         --  later entry into the choices table so that they can be sorted
         --  later on.

         procedure Check_Case_Pattern_Choices;
         --  Check choices validity for the Ada extension case where the
         --  selecting expression is not of a discrete type and so the
         --  choices are patterns.

         procedure Check_Composite_Case_Selector;
         --  Check that the (non-discrete) type of the expression being
         --  cased on is suitable.

         procedure Handle_Static_Predicate
           (Typ : Entity_Id;
            Lo  : Node_Id;
            Hi  : Node_Id);
         --  If the type of the alternative has predicates, we must examine
         --  each subset of the predicate rather than the bounds of the type
         --  itself. This is relevant when the choice is a subtype mark or a
         --  subtype indication.

         -----------
         -- Check --
         -----------

         procedure Check (Choice : Node_Id; Lo, Hi : Node_Id) is
            Lo_Val : Uint;
            Hi_Val : Uint;

         begin
            --  First check if an error was already detected on either bounds

            if Etype (Lo) = Any_Type or else Etype (Hi) = Any_Type then
               return;

            --  Do not insert non static choices in the table to be sorted

            elsif not Is_OK_Static_Expression (Lo)
                    or else
                  not Is_OK_Static_Expression (Hi)
            then
               Process_Non_Static_Choice (Choice);
               return;

            --  Ignore range which raise constraint error

            elsif Raises_Constraint_Error (Lo)
              or else Raises_Constraint_Error (Hi)
            then
               Raises_CE := True;
               return;

            --  AI05-0188 : Within an instance the non-others choices do not
            --  have to belong to the actual subtype.

            elsif Ada_Version >= Ada_2012 and then In_Instance then
               return;

            --  Otherwise we have an OK static choice

            else
               Lo_Val := Expr_Value (Lo);
               Hi_Val := Expr_Value (Hi);

               --  Do not insert null ranges in the choices table

               if Lo_Val > Hi_Val then
                  Process_Empty_Choice (Choice);
                  return;
               end if;
            end if;

            --  Check for low bound out of range

            if Lo_Val < Bounds_Lo then

               --  If the choice is an entity name, then it is a type, and we
               --  want to post the message on the reference to this entity.
               --  Otherwise post it on the lower bound of the range.

               if Is_Entity_Name (Choice) then
                  Enode := Choice;
               else
                  Enode := Lo;
               end if;

               --  Specialize message for integer/enum type

               if Is_Integer_Type (Bounds_Type) then
                  Error_Msg_Uint_1 := Bounds_Lo;
                  Error_Msg_N ("minimum allowed choice value is^", Enode);
               else
                  Error_Msg_Name_1 := Choice_Image (Bounds_Lo, Bounds_Type);
                  Error_Msg_N ("minimum allowed choice value is%", Enode);
               end if;
            end if;

            --  Check for high bound out of range

            if Hi_Val > Bounds_Hi then

               --  If the choice is an entity name, then it is a type, and we
               --  want to post the message on the reference to this entity.
               --  Otherwise post it on the upper bound of the range.

               if Is_Entity_Name (Choice) then
                  Enode := Choice;
               else
                  Enode := Hi;
               end if;

               --  Specialize message for integer/enum type

               if Is_Integer_Type (Bounds_Type) then
                  Error_Msg_Uint_1 := Bounds_Hi;
                  Error_Msg_N ("maximum allowed choice value is^", Enode);
               else
                  Error_Msg_Name_1 := Choice_Image (Bounds_Hi, Bounds_Type);
                  Error_Msg_N ("maximum allowed choice value is%", Enode);
               end if;
            end if;

            --  Collect bounds in the list

            --  Note: we still store the bounds, even if they are out of range,
            --  since this may prevent unnecessary cascaded errors for values
            --  that are covered by such an excessive range.

            Choice_List :=
              new Link'(Val => (Lo, Hi, Choice), Nxt => Choice_List);
            Num_Choices := Num_Choices + 1;
         end Check;

         --------------------------------
         -- Check_Case_Pattern_Choices --
         --------------------------------

         procedure Check_Case_Pattern_Choices is
            package Ops is new Composite_Case_Ops.Choice_Analysis
              (Case_Statement => N);
            use Ops;
            use Ops.Value_Sets;

            Empty : Value_Set renames Value_Sets.Empty;
            --  Cope with hiding due to multiple use clauses

            Info        : constant Choices_Info := Analysis;
            Others_Seen : Boolean := False;

         begin
            declare
               Matches : array (Alternative_Id) of Value_Sets.Value_Set :=
                 (others => Empty);

               Flag_Overlapping_Within_One_Alternative : constant Boolean :=
                 False;
               --  We may want to flag overlapping (perhaps with only a
               --  warning) if the pattern binds an identifier, as in
               --    when (Positive, <X>) | (Integer, <X>) =>

               Covered : Value_Set := Empty;
               --  The union of all alternatives seen so far
            begin
               if Composite_Case_Ops.Simplified_Composite_Coverage_Rules then
                  if not (for some Choice of Info => Choice.Is_Others) then
                     Error_Msg_N ("others choice required", N);
                  end if;
                  return;
               end if;

               for Choice of Info loop
                  if Choice.Is_Others then
                     Others_Seen := True;
                  else
                     if Flag_Overlapping_Within_One_Alternative
                        and then Compare (Matches (Choice.Alternative),
                                          Choice.Matches) /= Disjoint
                     then
                        Error_Msg_N
                          ("bad overlapping within one alternative", N);
                     end if;

                     Union (Target => Matches (Choice.Alternative),
                            Source => Choice.Matches);
                  end if;
               end loop;

               for A1 in Alternative_Id loop
                  for A2 in Alternative_Id
                              range A1 + 1 .. Alternative_Id'Last
                  loop
                     case Compare (Matches (A1), Matches (A2)) is
                        when Disjoint | Contained_By =>
                           null; -- OK
                        when Overlaps =>
                           declare
                              Uncovered_1, Uncovered_2 : Value_Set := Empty;
                           begin
                              Union (Uncovered_1, Matches (A1));
                              Remove (Uncovered_1, Covered);
                              Union (Uncovered_2, Matches (A2));
                              Remove (Uncovered_2, Covered);

                              --  Recheck for overlap after removing choices
                              --  covered by earlier alternatives.

                              case Compare (Uncovered_1, Uncovered_2) is
                                 when Disjoint | Contained_By =>
                                    null;
                                 when Contains | Overlaps | Equal =>
                                    Error_Msg_N
                                      ("bad alternative overlapping", N);
                              end case;
                           end;

                        when Equal =>
                           Error_Msg_N ("alternatives match same values", N);
                        when Contains =>
                           Error_Msg_N ("alternatives in wrong order", N);
                     end case;
                  end loop;

                  Union (Target => Covered, Source => Matches (A1));
               end loop;

               if not Others_Seen and then not Complement_Is_Empty (Covered)
               then
                  Error_Msg_N ("not all values are covered", N);
               end if;
            end;

            Ops.Value_Sets.Free_Value_Sets;
         end Check_Case_Pattern_Choices;

         -----------------------------------
         -- Check_Composite_Case_Selector --
         -----------------------------------

         procedure Check_Composite_Case_Selector is
         begin
            if not Is_Composite_Type (Subtyp) then
               Error_Msg_N
                 ("case selector type must be discrete or composite", N);
            elsif Is_Limited_Type (Subtyp) then
               Error_Msg_N ("case selector type must not be limited", N);
            elsif Is_Class_Wide_Type (Subtyp) then
               Error_Msg_N ("case selector type must not be class-wide", N);
            elsif Needs_Finalization (Subtyp)
              and then Is_Newly_Constructed
                         (Expression (N), Context_Requires_NC => False)
            then
               --  We could allow this case as long as there are no bindings.
               --
               --  If there are bindings, then allowing this case will get
               --  messy because the selector expression will be finalized
               --  before the statements of the selected alternative are
               --  executed (unless we add an INOX-specific change to the
               --  accessibility rules to prevent this earlier-than-wanted
               --  finalization, but adding new INOX-specific accessibility
               --  complexity is probably not the direction we want to go).
               --  This early selector finalization would be ok if we made
               --  copies in this case (so that the bindings would not yield
               --  a view of a finalized object), but then we'd have to deal
               --  with finalizing those copies (which would necessarily
               --  include defining their accessibility level). So it gets
               --  messy either way.

               Error_Msg_N ("case selector must not require finalization", N);
            end if;
         end Check_Composite_Case_Selector;

         -----------------------------
         -- Handle_Static_Predicate --
         -----------------------------

         procedure Handle_Static_Predicate
           (Typ : Entity_Id;
            Lo  : Node_Id;
            Hi  : Node_Id)
         is
            P : Node_Id;
            C : Node_Id;

         begin
            --  Loop through entries in predicate list, checking each entry.
            --  Note that if the list is empty, corresponding to a False
            --  predicate, then no choices are checked. If the choice comes
            --  from a subtype indication, the given range may have bounds
            --  that narrow the predicate choices themselves, so we must
            --  consider only those entries within the range of the given
            --  subtype indication..

            P := First (Static_Discrete_Predicate (Typ));
            while Present (P) loop

               --  Check that part of the predicate choice is included in the
               --  given bounds.

               if Expr_Value (High_Bound (P)) >= Expr_Value (Lo)
                 and then Expr_Value (Low_Bound (P)) <= Expr_Value (Hi)
               then
                  C := New_Copy (P);
                  Set_Sloc (C, Sloc (Choice));
                  Set_Original_Node (C, Choice);

                  if Expr_Value (Low_Bound (C)) < Expr_Value (Lo) then
                     Set_Low_Bound (C, Lo);
                  end if;

                  if Expr_Value (High_Bound (C)) > Expr_Value (Hi) then
                     Set_High_Bound (C, Hi);
                  end if;

                  Check (C, Low_Bound (C), High_Bound (C));
               end if;

               Next (P);
            end loop;

            Set_Has_SP_Choice (Alt);
         end Handle_Static_Predicate;

      --  Start of processing for Check_Choices

      begin
         Raises_CE      := False;
         Others_Present := False;

         --  If Subtyp is not a discrete type or there was some other error,
         --  then don't try any semantic checking on the choices since we have
         --  a complete mess.

         if not Is_Discrete_Type (Subtyp) or else Subtyp = Any_Type then

            --  Hold on, maybe it isn't a complete mess after all.

            if All_Extensions_Allowed and then Subtyp /= Any_Type then
               Check_Composite_Case_Selector;
               Check_Case_Pattern_Choices;
            end if;

            return;
         end if;

         --  If Subtyp is not a static subtype Ada 95 requires then we use the
         --  bounds of its base type to determine the values covered by the
         --  discrete choices.

         --  In Ada 2012, if the subtype has a nonstatic predicate the full
         --  range of the base type must be covered as well.

         if Is_OK_Static_Subtype (Subtyp) then
            if not Has_Predicates (Subtyp)
              or else Has_Static_Predicate (Subtyp)
            then
               Bounds_Type := Subtyp;
            else
               Bounds_Type := Choice_Type;
            end if;

         else
            Bounds_Type := Choice_Type;
         end if;

         --  Obtain static bounds of type, unless this is a generic formal
         --  discrete type for which all choices will be nonstatic.

         if not Is_Generic_Type (Root_Type (Bounds_Type))
           or else Ekind (Bounds_Type) /= E_Enumeration_Type
         then
            Bounds_Lo := Expr_Value (Type_Low_Bound (Bounds_Type));
            Bounds_Hi := Expr_Value (Type_High_Bound (Bounds_Type));
         end if;

         if Choice_Type = Universal_Integer then
            Expected_Type := Any_Integer;
         else
            Expected_Type := Choice_Type;
         end if;

         --  Now loop through the case alternatives or record variants

         Alt := First (Alternatives);
         while Present (Alt) loop

            --  If pragma, just analyze it

            if Nkind (Alt) = N_Pragma then
               Analyze (Alt);

            --  Otherwise we have an alternative. In most cases the semantic
            --  processing leaves the list of choices unchanged

            --  Check each choice against its base type

            else
               Choice := First (Discrete_Choices (Alt));
               while Present (Choice) loop
                  Kind := Nkind (Choice);

                  --  Choice is a Range

                  if Kind = N_Range
                    or else (Kind = N_Attribute_Reference
                              and then Attribute_Name (Choice) = Name_Range)
                  then
                     Check (Choice, Low_Bound (Choice), High_Bound (Choice));

                  --  Choice is a subtype name

                  elsif Is_Entity_Name (Choice)
                    and then Is_Type (Entity (Choice))
                  then
                     --  Check for inappropriate type

                     if not Covers (Expected_Type, Etype (Choice)) then
                        Wrong_Type (Choice, Choice_Type);

                     --  Type is OK, so check further

                     else
                        E := Entity (Choice);

                        --  Case of predicated subtype

                        if Has_Predicates (E) then

                           --  Use of nonstatic predicate is an error

                           if not Is_Discrete_Type (E)
                             or else not Has_Static_Predicate (E)
                             or else Has_Dynamic_Predicate_Aspect (E)
                             or else Has_Ghost_Predicate_Aspect (E)
                           then
                              Bad_Predicated_Subtype_Use
                                ("cannot use subtype& with non-static "
                                 & "predicate as case alternative",
                                 Choice, E, Suggest_Static => True);

                           --  Static predicate case. The bounds are those of
                           --  the given subtype.

                           else
                              Handle_Static_Predicate (E,
                                Type_Low_Bound (E), Type_High_Bound (E));
                           end if;

                        --  Not predicated subtype case

                        elsif not Is_OK_Static_Subtype (E) then
                           Process_Non_Static_Choice (Choice);
                        else
                           Check
                             (Choice, Type_Low_Bound (E), Type_High_Bound (E));
                        end if;
                     end if;

                  --  Choice is a subtype indication

                  elsif Kind = N_Subtype_Indication then
                     Resolve_Discrete_Subtype_Indication
                       (Choice, Expected_Type);

                     if Etype (Choice) /= Any_Type then
                        declare
                           C : constant Node_Id := Constraint (Choice);
                           R : constant Node_Id := Range_Expression (C);
                           L : constant Node_Id := Low_Bound (R);
                           H : constant Node_Id := High_Bound (R);

                        begin
                           E := Entity (Subtype_Mark (Choice));

                           if not Is_OK_Static_Subtype (E) then
                              Process_Non_Static_Choice (Choice);

                           else
                              if Is_OK_Static_Expression (L)
                                   and then
                                 Is_OK_Static_Expression (H)
                              then
                                 if Expr_Value (L) > Expr_Value (H) then
                                    Process_Empty_Choice (Choice);
                                 else
                                    if Is_Out_Of_Range (L, E) then
                                       Apply_Compile_Time_Constraint_Error
                                         (L, "static value out of range",
                                          CE_Range_Check_Failed);
                                    end if;

                                    if Is_Out_Of_Range (H, E) then
                                       Apply_Compile_Time_Constraint_Error
                                         (H, "static value out of range",
                                          CE_Range_Check_Failed);
                                    end if;
                                 end if;
                              end if;

                              --  Check applicable predicate values within the
                              --  bounds of the given range.

                              if Has_Static_Predicate (E) then
                                 Handle_Static_Predicate (E, L, H);

                              else
                                 Check (Choice, L, H);
                              end if;
                           end if;
                        end;
                     end if;

                  --  The others choice is only allowed for the last
                  --  alternative and as its only choice.

                  elsif Kind = N_Others_Choice then
                     if not (Choice = First (Discrete_Choices (Alt))
                              and then Choice = Last (Discrete_Choices (Alt))
                              and then Alt = Last (Alternatives))
                     then
                        Error_Msg_N
                          ("the choice OTHERS must appear alone and last",
                           Choice);
                        return;
                     end if;

                     Others_Present := True;
                     Others_Choice  := Choice;

                  --  Only other possibility is an expression

                  else
                     Check (Choice, Choice, Choice);
                  end if;

                  --  Move to next choice

                  Next (Choice);
               end loop;

               Process_Associated_Node (Alt);
            end if;

            Next (Alt);
         end loop;

         --  Now we can create the Choice_Table, since we know how long
         --  it needs to be so we can allocate exactly the right length.

         declare
            Choice_Table : Choice_Table_Type (0 .. Num_Choices);

         begin
            --  Now copy the items we collected in the linked list into this
            --  newly allocated table (leave entry 0 unused for sorting).

            declare
               T : Link_Ptr;
            begin
               for J in 1 .. Num_Choices loop
                  T := Choice_List;
                  Choice_List := T.Nxt;
                  Choice_Table (J) := T.Val;
                  Free (T);
               end loop;
            end;

            Check_Choice_Set
              (Choice_Table,
               Bounds_Type,
               Subtyp,
               Others_Present or else Choice_Type = Universal_Integer,
               N);

            --  If no others choice we are all done, otherwise we have one more
            --  step, which is to set the Others_Discrete_Choices field of the
            --  others choice (to contain all otherwise unspecified choices).
            --  Skip this if CE is known to be raised.

            if Others_Present and not Raises_CE then
               Expand_Others_Choice
                 (Case_Table    => Choice_Table,
                  Others_Choice => Others_Choice,
                  Choice_Type   => Bounds_Type);
            end if;
         end;
      end Check_Choices;

   end Generic_Check_Choices;

   -----------------------------------------
   --  Has_Static_Discriminant_Constraint --
   -----------------------------------------

   function Has_Static_Discriminant_Constraint
     (Subtyp : Entity_Id) return Boolean
   is
   begin
      if Has_Discriminants (Subtyp) and then Is_Constrained (Subtyp) then
         declare
            DC_Elmt : Elmt_Id := First_Elmt (Discriminant_Constraint (Subtyp));
         begin
            while Present (DC_Elmt) loop
               if not All_Composite_Constraints_Static (Node (DC_Elmt)) then
                  return False;
               end if;
               Next_Elmt (DC_Elmt);
            end loop;
            return True;
         end;
      end if;
      return False;
   end Has_Static_Discriminant_Constraint;

   ----------------------------
   -- Is_Case_Choice_Pattern --
   ----------------------------

   function Is_Case_Choice_Pattern (Expr : Node_Id) return Boolean is
      E : Node_Id := Expr;
   begin
      if not All_Extensions_Allowed then
         return False;
      end if;

      loop
         case Nkind (E) is
            when N_Case_Statement_Alternative
               | N_Case_Expression_Alternative
            =>
               --  We could return False if selecting expression is discrete,
               --  but this doesn't seem to be worth the bother.
               return True;

            when N_Empty
               | N_Statement_Other_Than_Procedure_Call
               | N_Procedure_Call_Statement
               | N_Declaration
            =>
               return False;

            when others =>
               E := Parent (E);
         end case;
      end loop;
   end Is_Case_Choice_Pattern;

end Sem_Case;

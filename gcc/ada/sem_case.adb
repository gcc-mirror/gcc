------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ C A S E                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--          Copyright (C) 1996-2002 Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Atree;    use Atree;
with Einfo;    use Einfo;
with Errout;   use Errout;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Sem;      use Sem;
with Sem_Eval; use Sem_Eval;
with Sem_Res;  use Sem_Res;
with Sem_Util; use Sem_Util;
with Sem_Type; use Sem_Type;
with Snames;   use Snames;
with Stand;    use Stand;
with Sinfo;    use Sinfo;
with Uintp;    use Uintp;

with GNAT.Heap_Sort_A; use GNAT.Heap_Sort_A;

package body Sem_Case is

   -----------------------
   -- Local Subprograms --
   -----------------------

   type Sort_Choice_Table_Type is array (Nat range <>) of Choice_Bounds;
   --  This new array type is used as the actual table type for sorting
   --  discrete choices. The reason for not using Choice_Table_Type, is that
   --  in Sort_Choice_Table_Type we reserve entry 0 for the sorting algortim
   --  (this is not absolutely necessary but it makes the code more
   --  efficient).

   procedure Check_Choices
     (Choice_Table   : in out Sort_Choice_Table_Type;
      Bounds_Type    : Entity_Id;
      Others_Present : Boolean;
      Msg_Sloc       : Source_Ptr);
   --  This is the procedure which verifies that a set of case statement,
   --  array aggregate or record variant choices has no duplicates, and
   --  covers the range specified by Bounds_Type. Choice_Table contains the
   --  discrete choices to check. These must start at position 1.
   --  Furthermore Choice_Table (0) must exist. This element is used by
   --  the sorting algorithm as a temporary. Others_Present is a flag
   --  indicating whether or not an Others choice is present. Finally
   --  Msg_Sloc gives the source location of the construct containing the
   --  choices in the Choice_Table.

   function Choice_Image (Value : Uint; Ctype : Entity_Id) return Name_Id;
   --  Given a Pos value of enumeration type Ctype, returns the name
   --  ID of an appropriate string to be used in error message output.

   -------------------
   -- Check_Choices --
   -------------------

   procedure Check_Choices
     (Choice_Table   : in out Sort_Choice_Table_Type;
      Bounds_Type    : Entity_Id;
      Others_Present : Boolean;
      Msg_Sloc       : Source_Ptr)
   is

      function Lt_Choice (C1, C2 : Natural) return Boolean;
      --  Comparison routine for comparing Choice_Table entries.
      --  Use the lower bound of each Choice as the key.

      procedure Move_Choice (From : Natural; To : Natural);
      --  Move routine for sorting the Choice_Table.

      procedure Issue_Msg (Value1 : Node_Id; Value2 : Node_Id);
      procedure Issue_Msg (Value1 : Node_Id; Value2 : Uint);
      procedure Issue_Msg (Value1 : Uint;    Value2 : Node_Id);
      procedure Issue_Msg (Value1 : Uint;    Value2 : Uint);
      --  Issue an error message indicating that there are missing choices,
      --  followed by the image of the missing choices themselves which lie
      --  between Value1 and Value2 inclusive.

      ---------------
      -- Issue_Msg --
      ---------------

      procedure Issue_Msg (Value1 : Node_Id; Value2 : Node_Id) is
      begin
         Issue_Msg (Expr_Value (Value1), Expr_Value (Value2));
      end Issue_Msg;

      procedure Issue_Msg (Value1 : Node_Id; Value2 : Uint) is
      begin
         Issue_Msg (Expr_Value (Value1), Value2);
      end Issue_Msg;

      procedure Issue_Msg (Value1 : Uint; Value2 : Node_Id) is
      begin
         Issue_Msg (Value1, Expr_Value (Value2));
      end Issue_Msg;

      procedure Issue_Msg (Value1 : Uint; Value2 : Uint) is
      begin
         --  In some situations, we call this with a null range, and
         --  obviously we don't want to complain in this case!

         if Value1 > Value2 then
            return;
         end if;

         --  Case of only one value that is missing

         if Value1 = Value2 then
            if Is_Integer_Type (Bounds_Type) then
               Error_Msg_Uint_1 := Value1;
               Error_Msg ("missing case value: ^!", Msg_Sloc);
            else
               Error_Msg_Name_1 := Choice_Image (Value1, Bounds_Type);
               Error_Msg ("missing case value: %!", Msg_Sloc);
            end if;

         --  More than one choice value, so print range of values

         else
            if Is_Integer_Type (Bounds_Type) then
               Error_Msg_Uint_1 := Value1;
               Error_Msg_Uint_2 := Value2;
               Error_Msg ("missing case values: ^ .. ^!", Msg_Sloc);
            else
               Error_Msg_Name_1 := Choice_Image (Value1, Bounds_Type);
               Error_Msg_Name_2 := Choice_Image (Value2, Bounds_Type);
               Error_Msg ("missing case values: % .. %!", Msg_Sloc);
            end if;
         end if;
      end Issue_Msg;

      ---------------
      -- Lt_Choice --
      ---------------

      function Lt_Choice (C1, C2 : Natural) return Boolean is
      begin
         return
           Expr_Value (Choice_Table (Nat (C1)).Lo)
           <= Expr_Value (Choice_Table (Nat (C2)).Lo);
      end Lt_Choice;

      -----------------
      -- Move_Choice --
      -----------------

      procedure Move_Choice (From : Natural; To : Natural) is
      begin
         Choice_Table (Nat (To)) := Choice_Table (Nat (From));
      end Move_Choice;

      --  Variables local to Check_Choices

      Choice      : Node_Id;
      Bounds_Lo   : constant Node_Id := Type_Low_Bound (Bounds_Type);
      Bounds_Hi   : constant Node_Id := Type_High_Bound (Bounds_Type);

      Prev_Choice : Node_Id;

      Hi       : Uint;
      Lo       : Uint;
      Prev_Hi  : Uint;

   --  Start processing for Check_Choices

   begin

      --  Choice_Table must start at 0 which is an unused location used
      --  by the sorting algorithm. However the first valid position for
      --  a discrete choice is 1.

      pragma Assert (Choice_Table'First = 0);

      if Choice_Table'Last = 0 then
         if not Others_Present then
            Issue_Msg (Bounds_Lo, Bounds_Hi);
         end if;
         return;
      end if;

      Sort
        (Positive (Choice_Table'Last),
         Move_Choice'Unrestricted_Access,
         Lt_Choice'Unrestricted_Access);

      Lo      := Expr_Value (Choice_Table (1).Lo);
      Hi      := Expr_Value (Choice_Table (1).Hi);
      Prev_Hi := Hi;

      if not Others_Present and then Expr_Value (Bounds_Lo) < Lo then
         Issue_Msg (Bounds_Lo, Lo - 1);
      end if;

      for J in 2 .. Choice_Table'Last loop
         Lo := Expr_Value (Choice_Table (J).Lo);
         Hi := Expr_Value (Choice_Table (J).Hi);

         if Lo <= Prev_Hi then
            Prev_Choice := Choice_Table (J - 1).Node;
            Choice      := Choice_Table (J).Node;

            if Sloc (Prev_Choice) <= Sloc (Choice) then
               Error_Msg_Sloc := Sloc (Prev_Choice);
               Error_Msg_N ("duplication of choice value#", Choice);
            else
               Error_Msg_Sloc := Sloc (Choice);
               Error_Msg_N ("duplication of choice value#", Prev_Choice);
            end if;

         elsif not Others_Present and then Lo /= Prev_Hi + 1 then
            Issue_Msg (Prev_Hi + 1, Lo - 1);
         end if;

         Prev_Hi := Hi;
      end loop;

      if not Others_Present and then Expr_Value (Bounds_Hi) > Hi then
         Issue_Msg (Hi + 1, Bounds_Hi);
      end if;
   end Check_Choices;

   ------------------
   -- Choice_Image --
   ------------------

   function Choice_Image (Value : Uint; Ctype : Entity_Id) return Name_Id is
      Rtp : constant Entity_Id := Root_Type (Ctype);
      Lit : Entity_Id;
      C   : Int;

   begin
      --  For character, or wide character. If we are in 7-bit ASCII graphic
      --  range, then build and return appropriate character literal name

      if Rtp = Standard_Character
        or else Rtp = Standard_Wide_Character
      then
         C := UI_To_Int (Value);

         if C in 16#20# .. 16#7E# then
            Set_Character_Literal_Name (Char_Code (UI_To_Int (Value)));
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
      Name_Len := Name_Len + 1;
      Name_Buffer (Name_Len) := ''';
      Name_Len := Name_Len + 1;
      Name_Buffer (Name_Len) := 'v';
      Name_Len := Name_Len + 1;
      Name_Buffer (Name_Len) := 'a';
      Name_Len := Name_Len + 1;
      Name_Buffer (Name_Len) := 'l';
      Name_Len := Name_Len + 1;
      Name_Buffer (Name_Len) := '(';

      UI_Image (Value);

      for J in 1 .. UI_Image_Length loop
         Name_Len := Name_Len + 1;
         Name_Buffer (Name_Len) := UI_Image_Buffer (J);
      end loop;

      Name_Len := Name_Len + 1;
      Name_Buffer (Name_Len) := ')';
      return Name_Find;
   end Choice_Image;

   -----------
   -- No_OP --
   -----------

   procedure No_OP (C : Node_Id) is
      pragma Warnings (Off, C);

   begin
      null;
   end No_OP;

   --------------------------------
   -- Generic_Choices_Processing --
   --------------------------------

   package body Generic_Choices_Processing is

      ---------------------
      -- Analyze_Choices --
      ---------------------

      procedure Analyze_Choices
        (N              : Node_Id;
         Subtyp         : Entity_Id;
         Choice_Table   : in out Choice_Table_Type;
         Last_Choice    : out Nat;
         Raises_CE      : out Boolean;
         Others_Present : out Boolean)
      is

         Nb_Choices        : constant Nat := Choice_Table'Length;
         Sort_Choice_Table : Sort_Choice_Table_Type (0 .. Nb_Choices);

         Choice_Type : constant Entity_Id := Base_Type (Subtyp);
         --  The actual type against which the discrete choices are
         --  resolved.  Note that this type is always the base type not the
         --  subtype of the ruling expression, index or discriminant.

         Bounds_Type : Entity_Id;
         --  The type from which are derived the bounds of the values
         --  covered by th discrete choices (see 3.8.1 (4)). If a discrete
         --  choice specifies a value outside of these bounds we have an error.

         Bounds_Lo   : Uint;
         Bounds_Hi   : Uint;
         --  The actual bounds of the above type.

         Expected_Type : Entity_Id;
         --  The expected type of each choice. Equal to Choice_Type, except
         --  if the expression is universal,  in which case the choices can
         --  be of any integer type.

         procedure Check (Choice : Node_Id; Lo, Hi : Node_Id);
         --  Checks the validity of the bounds of a choice.  When the bounds
         --  are static and no error occurred the bounds are entered into
         --  the choices table so that they can be sorted later on.

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

            elsif not Is_Static_Expression (Lo)
              or else not Is_Static_Expression (Hi)
            then
               Process_Non_Static_Choice (Choice);
               return;

            --  Ignore range which raise constraint error

            elsif Raises_Constraint_Error (Lo)
              or else Raises_Constraint_Error (Hi)
            then
               Raises_CE := True;
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

            --  Check for bound out of range.

            if Lo_Val < Bounds_Lo then
               if Is_Integer_Type (Bounds_Type) then
                  Error_Msg_Uint_1 := Bounds_Lo;
                  Error_Msg_N ("minimum allowed choice value is^", Lo);
               else
                  Error_Msg_Name_1 := Choice_Image (Bounds_Lo, Bounds_Type);
                  Error_Msg_N ("minimum allowed choice value is%", Lo);
               end if;

            elsif Hi_Val > Bounds_Hi then
               if Is_Integer_Type (Bounds_Type) then
                  Error_Msg_Uint_1 := Bounds_Hi;
                  Error_Msg_N ("maximum allowed choice value is^", Hi);
               else
                  Error_Msg_Name_1 := Choice_Image (Bounds_Hi, Bounds_Type);
                  Error_Msg_N ("maximum allowed choice value is%", Hi);
               end if;
            end if;

            --  We still store the bounds in the table, even if they are out
            --  of range, since this may prevent unnecessary cascaded errors
            --  for values that are covered by such an excessive range.

            Last_Choice := Last_Choice + 1;
            Sort_Choice_Table (Last_Choice).Lo   := Lo;
            Sort_Choice_Table (Last_Choice).Hi   := Hi;
            Sort_Choice_Table (Last_Choice).Node := Choice;
         end Check;

         --  Variables local to Analyze_Choices

         Alt : Node_Id;
         --  A case statement alternative, an array aggregate component
         --  association or a variant in a record type declaration

         Choice : Node_Id;
         Kind   : Node_Kind;
         --  The node kind of the current Choice.

         E : Entity_Id;

      --  Start of processing for Analyze_Choices

      begin
         Last_Choice    := 0;
         Raises_CE      := False;
         Others_Present := False;

         --  If Subtyp is not a static subtype Ada 95 requires then we use
         --  the bounds of its base type to determine the values covered by
         --  the discrete choices.

         if Is_OK_Static_Subtype (Subtyp) then
            Bounds_Type := Subtyp;
         else
            Bounds_Type := Choice_Type;
         end if;

         --  Obtain static bounds of type, unless this is a generic formal
         --  discrete type for which all choices will be non-static.

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

         --  Now loop through the case statement alternatives or array
         --  aggregate component associations or record variants.

         Alt := First (Get_Alternatives (N));
         while Present (Alt) loop

            --  If pragma, just analyze it

            if Nkind (Alt) = N_Pragma then
               Analyze (Alt);

            --  Otherwise check each choice against its base type

            else
               Choice := First (Get_Choices (Alt));

               while Present (Choice) loop
                  Analyze (Choice);
                  Kind := Nkind (Choice);

                  --  Choice is a Range

                  if Kind = N_Range
                    or else (Kind = N_Attribute_Reference
                             and then Attribute_Name (Choice) = Name_Range)
                  then
                     Resolve (Choice, Expected_Type);
                     Check (Choice, Low_Bound (Choice), High_Bound (Choice));

                  --  Choice is a subtype name

                  elsif Is_Entity_Name (Choice)
                    and then Is_Type (Entity (Choice))
                  then
                     if not Covers (Expected_Type, Etype (Choice)) then
                        Wrong_Type (Choice, Choice_Type);

                     else
                        E := Entity (Choice);

                        if not Is_Static_Subtype (E) then
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

                           if not Is_Static_Subtype (E) then
                              Process_Non_Static_Choice (Choice);

                           else
                              if Is_OK_Static_Expression (L)
                                and then Is_OK_Static_Expression (H)
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

                              Check (Choice, L, H);
                           end if;
                        end;
                     end if;

                  --  The others choice is only allowed for the last
                  --  alternative and as its only choice.

                  elsif Kind = N_Others_Choice then
                     if not (Choice = First (Get_Choices (Alt))
                             and then Choice = Last (Get_Choices (Alt))
                             and then Alt = Last (Get_Alternatives (N)))
                     then
                        Error_Msg_N
                          ("the choice OTHERS must appear alone and last",
                           Choice);
                        return;
                     end if;

                     Others_Present := True;

                  --  Only other possibility is an expression

                  else
                     Resolve (Choice, Expected_Type);
                     Check (Choice, Choice, Choice);
                  end if;

                  Next (Choice);
               end loop;

               Process_Associated_Node (Alt);
            end if;

            Next (Alt);
         end loop;

         Check_Choices
           (Sort_Choice_Table (0 .. Last_Choice),
            Bounds_Type,
            Others_Present or else (Choice_Type = Universal_Integer),
            Sloc (N));

         --  Now copy the sorted discrete choices

         for J in 1 .. Last_Choice loop
            Choice_Table (Choice_Table'First - 1 + J) := Sort_Choice_Table (J);
         end loop;

      end Analyze_Choices;

      -----------------------
      -- Number_Of_Choices --
      -----------------------

      function Number_Of_Choices (N : Node_Id) return Nat is
         Alt : Node_Id;
         --  A case statement alternative, an array aggregate component
         --  association or a record variant.

         Choice : Node_Id;
         Count  : Nat := 0;

      begin
         if not Present (Get_Alternatives (N)) then
            return 0;
         end if;

         Alt := First_Non_Pragma (Get_Alternatives (N));
         while Present (Alt) loop

            Choice := First (Get_Choices (Alt));
            while Present (Choice) loop
               if Nkind (Choice) /= N_Others_Choice then
                  Count := Count + 1;
               end if;

               Next (Choice);
            end loop;

            Next_Non_Pragma (Alt);
         end loop;

         return Count;
      end Number_Of_Choices;

   end Generic_Choices_Processing;

end Sem_Case;

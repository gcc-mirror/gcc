------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               P P R I N T                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2008-2012, Free Software Foundation, Inc.         --
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

with Atree;   use Atree;
with Einfo;   use Einfo;
with Namet;   use Namet;
with Nlists;  use Nlists;
with Opt;     use Opt;
with Sinfo;   use Sinfo;
with Sinput;  use Sinput;
with Snames;  use Snames;
with Uintp;   use Uintp;

package body Pprint is

   List_Name_Count : Integer := 0;
   --  Counter used to prevent infinite recursion while computing name of
   --  complex expressions.

   ----------------------
   -- Expression_Image --
   ----------------------

   function Expression_Image (Expr : Node_Id; Default : String)
      return String is
      Left         : Node_Id := Original_Node (Expr);
      Right        : Node_Id := Original_Node (Expr);
      From_Source  : constant Boolean :=
        Comes_From_Source (Expr) and then not Opt.Debug_Generated_Code;
      Append_Paren : Boolean := False;

      function Expr_Name
        (Expr        : Node_Id;
         Take_Prefix : Boolean := True;
         Expand_Type : Boolean := True) return String;
      --  Return string corresponding to Expr. If no string can be extracted,
      --  return "...". If Take_Prefix is True, go back to prefix when needed,
      --  otherwise only consider the right-hand side of an expression. If
      --  Expand_Type is True and Expr is a type, try to expand Expr (an
      --  internally generated type) into a user understandable name.

      Max_List : constant := 3;
      --  Limit number of list elements to dump

      Max_Expr_Elements : constant := 24;
      --  Limit number of elements in an expression for use by Expr_Name

      Num_Elements : Natural := 0;
      --  Current number of elements processed by Expr_Name

      function List_Name
        (List      : Node_Id;
         Add_Space : Boolean := True;
         Add_Paren : Boolean := True) return String;
      --  Return a string corresponding to List

      function List_Name
        (List      : Node_Id;
         Add_Space : Boolean := True;
         Add_Paren : Boolean := True) return String
      is
         function Internal_List_Name
           (List      : Node_Id;
            First     : Boolean := True;
            Add_Space : Boolean := True;
            Add_Paren : Boolean := True;
            Num       : Natural := 1) return String;

         ------------------------
         -- Internal_List_Name --
         ------------------------

         function Internal_List_Name
           (List      : Node_Id;
            First     : Boolean := True;
            Add_Space : Boolean := True;
            Add_Paren : Boolean := True;
            Num       : Natural := 1) return String
         is
            function Prepend (S : String) return String;

            -------------
            -- Prepend --
            -------------

            function Prepend (S : String) return String is
            begin
               if Add_Space then
                  if Add_Paren then
                     return " (" & S;
                  else
                     return ' ' & S;
                  end if;
               elsif Add_Paren then
                  return '(' & S;
               else
                  return S;
               end if;
            end Prepend;

         --  Start of processing for Internal_List_Name

         begin
            if not Present (List) then
               if First or else not Add_Paren then
                  return "";
               else
                  return ")";
               end if;
            elsif Num > Max_List then
               if Add_Paren then
                  return ", ...)";
               else
                  return ", ...";
               end if;
            end if;

            if First then
               return Prepend
                 (Expr_Name (List)
                  & Internal_List_Name (Next (List),
                    First     => False,
                    Add_Paren => Add_Paren,
                    Num       => Num + 1));
            else
               return ", " & Expr_Name (List) &
                 Internal_List_Name
                 (Next (List),
                  First     => False,
                  Add_Paren => Add_Paren,
                  Num       => Num + 1);
            end if;
         end Internal_List_Name;

      --  Start of processing for List_Name

      begin
         --  Prevent infinite recursion by limiting depth to 3

         if List_Name_Count > 3 then
            return "...";
         end if;

         List_Name_Count := List_Name_Count + 1;
         declare
            Result : constant String :=
              Internal_List_Name
                (List, Add_Space => Add_Space, Add_Paren => Add_Paren);
         begin
            List_Name_Count := List_Name_Count - 1;
            return Result;
         end;
      end List_Name;

      ---------------
      -- Expr_Name --
      ---------------

      function Expr_Name
        (Expr        : Node_Id;
         Take_Prefix : Boolean := True;
         Expand_Type : Boolean := True) return String
      is
      begin
         Num_Elements := Num_Elements + 1;

         if Num_Elements > Max_Expr_Elements then
            return "...";
         end if;

         case Nkind (Expr) is
            when N_Defining_Identifier | N_Identifier =>
               return Ident_Image (Expr, Expression_Image.Expr, Expand_Type);

            when N_Character_Literal =>
               declare
                  Char : constant Int :=
                    UI_To_Int (Char_Literal_Value (Expr));
               begin
                  if Char in 32 .. 127 then
                     return "'" & Character'Val (Char) & "'";
                  else
                     UI_Image (Char_Literal_Value (Expr));
                     return "'\" & UI_Image_Buffer (1 .. UI_Image_Length)
                       & "'";
                  end if;
               end;

            when N_Integer_Literal =>
               UI_Image (Intval (Expr));
               return UI_Image_Buffer (1 .. UI_Image_Length);

            when N_Real_Literal =>
               return Real_Image (Realval (Expr));

            when N_String_Literal =>
               return String_Image (Strval (Expr));

            when N_Allocator =>
               return "new " & Expr_Name (Expression (Expr));

            when N_Aggregate =>
               if Present (Sinfo.Expressions (Expr)) then
                  return List_Name
                    (First (Sinfo.Expressions (Expr)), Add_Space => False);

               elsif Null_Record_Present (Expr) then
                  return ("(null record)");

               else
                  return List_Name
                    (First (Component_Associations (Expr)),
                     Add_Space => False, Add_Paren => False);
               end if;

            when N_Extension_Aggregate =>
               return "(" & Expr_Name (Ancestor_Part (Expr)) &
                 " with " &
                 List_Name (First (Sinfo.Expressions (Expr)),
                            Add_Space => False, Add_Paren => False) &
                 ")";

            when N_Attribute_Reference =>
               if Take_Prefix then
                  declare
                     Str    : constant String := Expr_Name (Prefix (Expr))
                       & "'" & Get_Name_String (Attribute_Name (Expr));
                     Id     : constant Attribute_Id :=
                       Get_Attribute_Id (Attribute_Name (Expr));
                     Ranges : List_Id;
                     N      : Node_Id;

                  begin
                     if (Id = Attribute_First or else Id = Attribute_Last)
                       and then Str (Str'First) = '$'
                     then
                        N := Associated_Node_For_Itype (Etype (Prefix (Expr)));

                        if Present (N) then
                           if Nkind (N) = N_Full_Type_Declaration then
                              N := Type_Definition (N);
                           end if;

                           if Nkind (N) = N_Subtype_Declaration then
                              Ranges := Constraints (Constraint
                                                     (Subtype_Indication (N)));

                              if List_Length (Ranges) = 1
                                and then Nkind_In
                                  (First (Ranges),
                                   N_Range,
                                   N_Real_Range_Specification,
                                   N_Signed_Integer_Type_Definition)
                              then
                                 if Id = Attribute_First then
                                    return Expression_Image
                                      (Low_Bound (First (Ranges)), Str);
                                 else
                                    return Expression_Image
                                      (High_Bound (First (Ranges)), Str);
                                 end if;
                              end if;
                           end if;
                        end if;
                     end if;

                     return Str;
                  end;
               else
                  return "'" & Get_Name_String (Attribute_Name (Expr));
               end if;

            when N_Explicit_Dereference =>
               if Take_Prefix then
                  return Expr_Name (Prefix (Expr)) & ".all";
               else
                  return ".all";
               end if;

            when N_Expanded_Name | N_Selected_Component =>
               if Take_Prefix then
                  return Expr_Name (Prefix (Expr))
                    & "." & Expr_Name (Selector_Name (Expr));
               else
                  return "." & Expr_Name (Selector_Name (Expr));
               end if;

            when N_Component_Association =>
               return "("
                 & List_Name (First (Choices (Expr)),
                              Add_Space => False, Add_Paren => False)
                 & " => " & Expr_Name (Expression (Expr)) & ")";

            when N_If_Expression =>
               declare
                  N : constant Node_Id := First (Sinfo.Expressions (Expr));
               begin
                  return "if " & Expr_Name (N) & " then " &
                    Expr_Name (Next (N)) & " else " &
                    Expr_Name (Next (Next (N)));
               end;

            when N_Qualified_Expression =>
               declare
                  Mark : constant String :=
                    Expr_Name (Subtype_Mark (Expr), Expand_Type => False);
                  Str  : constant String := Expr_Name (Expression (Expr));
               begin
                  if Str (Str'First) = '(' and then Str (Str'Last) = ')' then
                     return Mark & "'" & Str;
                  else
                     return Mark & "'(" & Str & ")";
                  end if;
               end;

            when N_Unchecked_Expression | N_Expression_With_Actions =>
               return Expr_Name (Expression (Expr));

            when N_Raise_Constraint_Error =>
               if Present (Condition (Expr)) then
                  return "[constraint_error when " &
                    Expr_Name (Condition (Expr)) & "]";
               else
                  return "[constraint_error]";
               end if;

            when N_Raise_Program_Error =>
               if Present (Condition (Expr)) then
                  return "[program_error when " &
                    Expr_Name (Condition (Expr)) & "]";
               else
                  return "[program_error]";
               end if;

            when N_Range =>
               return Expr_Name (Low_Bound (Expr)) & ".." &
                 Expr_Name (High_Bound (Expr));

            when N_Slice =>
               return Expr_Name (Prefix (Expr)) & " (" &
                 Expr_Name (Discrete_Range (Expr)) & ")";

            when N_And_Then =>
               return Expr_Name (Left_Opnd (Expr)) & " and then " &
                 Expr_Name (Right_Opnd (Expr));

            when N_In =>
               return Expr_Name (Left_Opnd (Expr)) & " in " &
                 Expr_Name (Right_Opnd (Expr));

            when N_Not_In =>
               return Expr_Name (Left_Opnd (Expr)) & " not in " &
                 Expr_Name (Right_Opnd (Expr));

            when N_Or_Else =>
               return Expr_Name (Left_Opnd (Expr)) & " or else " &
                 Expr_Name (Right_Opnd (Expr));

            when N_Op_And =>
               return Expr_Name (Left_Opnd (Expr)) & " and " &
                 Expr_Name (Right_Opnd (Expr));

            when N_Op_Or =>
               return Expr_Name (Left_Opnd (Expr)) & " or " &
                 Expr_Name (Right_Opnd (Expr));

            when N_Op_Xor =>
               return Expr_Name (Left_Opnd (Expr)) & " xor " &
                 Expr_Name (Right_Opnd (Expr));

            when N_Op_Eq =>
               return Expr_Name (Left_Opnd (Expr)) & " = " &
                 Expr_Name (Right_Opnd (Expr));

            when N_Op_Ne =>
               return Expr_Name (Left_Opnd (Expr)) & " /= " &
                 Expr_Name (Right_Opnd (Expr));

            when N_Op_Lt =>
               return Expr_Name (Left_Opnd (Expr)) & " < " &
                 Expr_Name (Right_Opnd (Expr));

            when N_Op_Le =>
               return Expr_Name (Left_Opnd (Expr)) & " <= " &
                 Expr_Name (Right_Opnd (Expr));

            when N_Op_Gt =>
               return Expr_Name (Left_Opnd (Expr)) & " > " &
                 Expr_Name (Right_Opnd (Expr));

            when N_Op_Ge =>
               return Expr_Name (Left_Opnd (Expr)) & " >= " &
                 Expr_Name (Right_Opnd (Expr));

            when N_Op_Add =>
               return Expr_Name (Left_Opnd (Expr)) & " + " &
                 Expr_Name (Right_Opnd (Expr));

            when N_Op_Subtract =>
               return Expr_Name (Left_Opnd (Expr)) & " - " &
                 Expr_Name (Right_Opnd (Expr));

            when N_Op_Multiply =>
               return Expr_Name (Left_Opnd (Expr)) & " * " &
                 Expr_Name (Right_Opnd (Expr));

            when N_Op_Divide =>
               return Expr_Name (Left_Opnd (Expr)) & " / " &
                 Expr_Name (Right_Opnd (Expr));

            when N_Op_Mod =>
               return Expr_Name (Left_Opnd (Expr)) & " mod " &
                 Expr_Name (Right_Opnd (Expr));

            when N_Op_Rem =>
               return Expr_Name (Left_Opnd (Expr)) & " rem " &
                 Expr_Name (Right_Opnd (Expr));

            when N_Op_Expon =>
               return Expr_Name (Left_Opnd (Expr)) & " ** " &
                 Expr_Name (Right_Opnd (Expr));

            when N_Op_Shift_Left =>
               return Expr_Name (Left_Opnd (Expr)) & " << " &
                 Expr_Name (Right_Opnd (Expr));

            when N_Op_Shift_Right | N_Op_Shift_Right_Arithmetic =>
               return Expr_Name (Left_Opnd (Expr)) & " >> " &
                 Expr_Name (Right_Opnd (Expr));

            when N_Op_Concat =>
               return Expr_Name (Left_Opnd (Expr)) & " & " &
                 Expr_Name (Right_Opnd (Expr));

            when N_Op_Plus =>
               return "+" & Expr_Name (Right_Opnd (Expr));

            when N_Op_Minus =>
               return "-" & Expr_Name (Right_Opnd (Expr));

            when N_Op_Abs =>
               return "abs " & Expr_Name (Right_Opnd (Expr));

            when N_Op_Not =>
               return "not (" & Expr_Name (Right_Opnd (Expr)) & ")";

            when N_Parameter_Association =>
               return Expr_Name (Explicit_Actual_Parameter (Expr));

            when N_Type_Conversion | N_Unchecked_Type_Conversion =>

               --  Most conversions are not very interesting (used inside
               --  expanded checks to convert to larger ranges), so skip them.

               return Expr_Name (Expression (Expr));

            when N_Indexed_Component =>
               if Take_Prefix then
                  return Expr_Name (Prefix (Expr)) &
                    List_Name (First (Sinfo.Expressions (Expr)));
               else
                  return List_Name (First (Sinfo.Expressions (Expr)));
               end if;

            when N_Function_Call =>

               --  If Default = "", it means we're expanding the name of
               --  a gnat temporary (and not really a function call), so add
               --  parentheses around function call to mark it specially.

               if Default = "" then
                  return '(' & Expr_Name (Name (Expr)) &
                    List_Name (First (Sinfo.Parameter_Associations (Expr))) &
                      ')';
               else
                  return Expr_Name (Name (Expr)) &
                    List_Name (First (Sinfo.Parameter_Associations (Expr)));
               end if;

            when N_Null =>
               return "null";

            when N_Others_Choice =>
               return "others";

            when others =>
               return "...";
         end case;
      end Expr_Name;

   --  Start of processing for Expression_Name

   begin
      if not From_Source then
         declare
            S : constant String := Expr_Name (Expr);
         begin
            if S = "..." then
               return Default;
            else
               return S;
            end if;
         end;
      end if;

      --  Compute left (start) and right (end) slocs for the expression
      --  Consider using Sinput.Sloc_Range instead, except that it does not
      --  work properly currently???

      loop
         case Nkind (Left) is
            when N_Binary_Op | N_Membership_Test |
                 N_And_Then  | N_Or_Else         =>
               Left := Original_Node (Left_Opnd (Left));

            when N_Attribute_Reference  | N_Expanded_Name      |
                 N_Explicit_Dereference | N_Indexed_Component  |
                 N_Reference            | N_Selected_Component |
                 N_Slice                                       =>
               Left := Original_Node (Prefix (Left));

            when N_Designator | N_Defining_Program_Unit_Name |
                 N_Function_Call                             =>
               Left := Original_Node (Name (Left));

            when N_Range =>
               Left := Original_Node (Low_Bound (Left));

            when N_Type_Conversion =>
               Left := Original_Node (Subtype_Mark (Left));

            --  For any other item, quit loop

            when others =>
               exit;
         end case;
      end loop;

      loop
         case Nkind (Right) is
            when N_Op       | N_Membership_Test |
                 N_And_Then | N_Or_Else         =>
               Right := Original_Node (Right_Opnd (Right));

            when N_Selected_Component | N_Expanded_Name =>
               Right := Original_Node (Selector_Name (Right));

            when N_Designator =>
               Right := Original_Node (Identifier (Right));

            when N_Defining_Program_Unit_Name =>
               Right := Original_Node (Defining_Identifier (Right));

            when N_Range =>
               Right := Original_Node (High_Bound (Right));

            when N_Parameter_Association =>
               Right := Original_Node (Explicit_Actual_Parameter (Right));

            when N_Indexed_Component =>
               Right := Original_Node (Last (Sinfo.Expressions (Right)));
               Append_Paren := True;

            when N_Function_Call =>
               if Present (Sinfo.Parameter_Associations (Right)) then
                  Right :=
                    Original_Node
                      (Last (Sinfo.Parameter_Associations (Right)));
                  Append_Paren := True;

               --  Quit loop if no named associations

               else
                  exit;
               end if;

            --  For all other items, quit the loop

            when others =>
               exit;
         end case;
      end loop;

      declare
         Scn      : Source_Ptr := Original_Location (Sloc (Left));
         Src      : constant Source_Buffer_Ptr :=
           Source_Text (Get_Source_File_Index (Scn));
         End_Sloc : constant Source_Ptr :=
           Original_Location (Sloc (Right));

      begin
         if Scn > End_Sloc then
            return Default;
         end if;

         declare
            Buffer           : String (1 .. Natural (End_Sloc - Scn));
            Skipping_Comment : Boolean := False;
            Underscore       : Boolean := False;
            Index            : Natural := 0;

         begin
            if Right /= Expr then
               while Scn < End_Sloc loop
                  case Src (Scn) is
                  when ' ' | ASCII.HT =>
                     if not Skipping_Comment and then not Underscore then
                        Underscore := True;
                        Index := Index + 1;
                        Buffer (Index) := ' ';
                     end if;

                  --  CR/LF/FF is the end of any comment

                  when ASCII.LF | ASCII.CR | ASCII.FF =>
                     Skipping_Comment := False;

                  when others =>
                     Underscore := False;

                     if not Skipping_Comment then

                        --  Ignore comment

                        if Src (Scn) = '-' and then Src (Scn + 1) = '-' then
                           Skipping_Comment := True;

                        else
                           Index := Index + 1;
                           Buffer (Index) := Src (Scn);
                        end if;
                     end if;
                  end case;

                  Scn := Scn + 1;
               end loop;
            end if;

            if Index < 1 then
               declare
                  S : constant String := Expr_Name (Right);
               begin
                  if S = "..." then
                     return Default;
                  else
                     return S;
                  end if;
               end;

            elsif Append_Paren then
               return Buffer (1 .. Index) & Expr_Name (Right, False) & ')';

            else
               return Buffer (1 .. Index) & Expr_Name (Right, False);
            end if;
         end;
      end;
   end Expression_Image;

end Pprint;

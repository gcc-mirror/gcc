------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               P P R I N T                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2008-2024, Free Software Foundation, Inc.         --
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
with Errout;         use Errout;
with Namet;          use Namet;
with Nlists;         use Nlists;
with Opt;            use Opt;
with Sinfo;          use Sinfo;
with Sinfo.Nodes;    use Sinfo.Nodes;
with Sinfo.Utils;    use Sinfo.Utils;
with Sinput;         use Sinput;
with Snames;         use Snames;
with Uintp;          use Uintp;

with System.Case_Util;

package body Pprint is

   List_Name_Count : Natural := 0;
   --  Counter used to prevent infinite recursion while computing name of
   --  complex expressions.

   ----------------------
   -- Expression_Image --
   ----------------------

   function Expression_Image
     (Expr    : Node_Id;
      Default : String) return String
   is
      function Expr_Name
        (Expr        : Node_Id;
         Take_Prefix : Boolean := True;
         Expand_Type : Boolean := True) return String;
      --  Return string corresponding to Expr. If no string can be extracted,
      --  return "...". If Take_Prefix is True, go back to prefix when needed,
      --  otherwise only consider the right-hand side of an expression. If
      --  Expand_Type is True and Expr is a type, try to expand Expr (an
      --  internally generated type) into a user understandable name.

      function Count_Parentheses (S : String; C : Character) return Natural
        with Pre => C in '(' | ')';
      --  Returns the number of times parenthesis character C should be added
      --  to string S for getting a correctly parenthesized result. For C = '('
      --  this means prepending the character, for C = ')' this means appending
      --  the character.

      function Fix_Parentheses (S : String) return String;
      --  Counts the number of required opening and closing parentheses in S to
      --  respectively prepend and append for getting correct parentheses. Then
      --  returns S with opening parentheses prepended and closing parentheses
      --  appended so that the result is correctly parenthesized.

      Max_List_Depth : constant := 3;
      --  Limit number of nested lists to print

      Max_List_Length : constant := 3;
      --  Limit number of list elements to print

      Max_Expr_Elements : constant := 24;
      --  Limit number of elements in an expression for use by Expr_Name

      Num_Elements : Natural := 0;
      --  Current number of elements processed by Expr_Name

      function List_Name (List : List_Id) return String;
      --  Return a string corresponding to List

      ---------------
      -- List_Name --
      ---------------

      function List_Name (List : List_Id) return String is
         Buf  : Bounded_String;
         Elmt : Node_Id;

         Printed_Elmts : Natural := 0;

      begin
         --  Give up if the printed list is too deep

         if List_Name_Count > Max_List_Depth then
            return "...";
         end if;

         List_Name_Count := List_Name_Count + 1;

         Elmt := First (List);
         while Present (Elmt) loop

            --  Print component_association as "x | y | z => 12345"

            if Nkind (Elmt) = N_Component_Association then
               declare
                  Choice : Node_Id := First (Choices (Elmt));
               begin
                  while Present (Choice) loop
                     Append (Buf, Expr_Name (Choice));
                     Next (Choice);

                     if Present (Choice) then
                        Append (Buf, " | ");
                     end if;
                  end loop;
               end;
               Append (Buf, " => ");
               if Box_Present (Elmt) then
                  Append (Buf, "<>");
               else
                  Append (Buf, Expr_Name (Expression (Elmt)));
               end if;

            --  Print parameter_association as "x => 12345"

            elsif Nkind (Elmt) = N_Parameter_Association then
               Append (Buf, Expr_Name (Selector_Name (Elmt)));
               Append (Buf, " => ");
               Append (Buf, Expr_Name (Explicit_Actual_Parameter (Elmt)));

            --  Print expression itself as "12345"

            else
               Append (Buf, Expr_Name (Elmt));
            end if;

            Next (Elmt);
            Printed_Elmts := Printed_Elmts + 1;

            --  Separate next element with a comma, if necessary

            if Present (Elmt) then
               Append (Buf, ", ");

               --  Abbreviate remaining elements as "...", if limit exceeded

               if Printed_Elmts = Max_List_Length then
                  Append (Buf, "...");
                  exit;
               end if;
            end if;
         end loop;

         List_Name_Count := List_Name_Count - 1;

         return To_String (Buf);
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

         --  Just print pieces of aggregate nodes, even though they are not
         --  expressions. It is too much trouble to handle them any better.

         if Nkind (Expr) = N_Component_Association then

            pragma Assert (Box_Present (Expr));

            declare
               Buf    : Bounded_String;
               Choice : Node_Id := First (Choices (Expr));
            begin
               while Present (Choice) loop
                  Append (Buf, Expr_Name (Choice));
                  Next (Choice);

                  if Present (Choice) then
                     Append (Buf, " | ");
                  end if;
               end loop;

               Append (Buf, " => <>");

               return To_String (Buf);
            end;

         elsif Nkind (Expr) = N_Others_Choice then
            return "others";
         end if;

         case N_Subexpr'(Nkind (Expr)) is
            when N_Identifier =>
               return Ident_Image (Expr, Expression_Image.Expr, Expand_Type);

            when N_Character_Literal =>
               declare
                  Char : constant Int := UI_To_Int (Char_Literal_Value (Expr));
               begin
                  if Char in 32 .. 126 then
                     return "'" & Character'Val (Char) & "'";
                  else
                     UI_Image (Char_Literal_Value (Expr));
                     return
                       "'\" & UI_Image_Buffer (1 .. UI_Image_Length) & "'";
                  end if;
               end;

            when N_Integer_Literal =>
               return UI_Image (Intval (Expr));

            when N_Real_Literal =>
               return Real_Image (Realval (Expr));

            when N_String_Literal =>
               return String_Image (Strval (Expr));

            when N_Allocator =>
               return "new " & Expr_Name (Expression (Expr));

            when N_Aggregate =>
               if Present (Expressions (Expr)) then
                  return '(' & List_Name (Expressions (Expr)) & ')';

               --  Do not return empty string for (others => <>) aggregate
               --  of a componentless record type. At least one caller (the
               --  recursive call below in the N_Qualified_Expression case)
               --  is not prepared to deal with a zero-length result.

               elsif Null_Record_Present (Expr)
                 or else No (First (Component_Associations (Expr)))
               then
                  return ("(null record)");

               else
                  return '(' & List_Name (Component_Associations (Expr)) & ')';
               end if;

            when N_Extension_Aggregate =>
               return '(' & Expr_Name (Ancestor_Part (Expr))
                 & " with (" & List_Name (Expressions (Expr)) & ')';

            when N_Attribute_Reference =>
               if Take_Prefix then
                  declare
                     Id : constant Attribute_Id :=
                            Get_Attribute_Id (Attribute_Name (Expr));

                     --  Always use mixed case for attributes

                     Str : constant String :=
                             Expr_Name (Prefix (Expr))
                               & "'"
                               & System.Case_Util.To_Mixed
                                   (Get_Name_String (Attribute_Name (Expr)));

                     N      : Node_Id;
                     Ranges : List_Id;

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
                              Ranges :=
                                Constraints
                                  (Constraint (Subtype_Indication (N)));

                              if List_Length (Ranges) = 1
                                and then Nkind (First (Ranges)) in
                                           N_Range                          |
                                           N_Real_Range_Specification       |
                                           N_Signed_Integer_Type_Definition
                              then
                                 if Id = Attribute_First then
                                    return
                                      Expression_Image
                                        (Low_Bound (First (Ranges)), Str);
                                 else
                                    return
                                      Expression_Image
                                        (High_Bound (First (Ranges)), Str);
                                 end if;
                              end if;
                           end if;
                        end if;
                     end if;

                     return Str;
                  end;
               else
                  return ''' & Get_Name_String (Attribute_Name (Expr));
               end if;

            when N_Explicit_Dereference =>
               Explicit_Dereference : declare
                  function Deref_Suffix return String;
                  --  Usually returns ".all", but will return "" if
                  --  Hide_Temp_Derefs is true and the prefix is a use of a
                  --  not-from-source object declared as
                  --    X : constant Some_Access_Type := Some_Expr'Reference;
                  --  (as is sometimes done in Exp_Util.Remove_Side_Effects).

                  ------------------
                  -- Deref_Suffix --
                  ------------------

                  function Deref_Suffix return String is
                     Decl : Node_Id;

                  begin
                     if Hide_Temp_Derefs
                       and then Nkind (Prefix (Expr)) = N_Identifier
                       and then Nkind (Entity (Prefix (Expr))) =
                                  N_Defining_Identifier
                     then
                        Decl := Parent (Entity (Prefix (Expr)));

                        if Present (Decl)
                          and then Nkind (Decl) = N_Object_Declaration
                          and then not Comes_From_Source (Decl)
                          and then Constant_Present (Decl)
                          and then Present (Expression (Decl))
                          and then Nkind (Expression (Decl)) = N_Reference
                        then
                           return "";
                        end if;
                     end if;

                     --  The default case

                     return ".all";
                  end Deref_Suffix;

               --  Start of processing for Explicit_Dereference

               begin
                  if Hide_Parameter_Blocks
                    and then Nkind (Prefix (Expr)) = N_Selected_Component
                    and then Present (Etype (Prefix (Expr)))
                    and then Is_Access_Type (Etype (Prefix (Expr)))
                    and then Is_Param_Block_Component_Type
                               (Etype (Prefix (Expr)))
                  then
                     --  Return "Foo" instead of "Parameter_Block.Foo.all"

                     return Expr_Name (Selector_Name (Prefix (Expr)));

                  elsif Take_Prefix then
                     return Expr_Name (Prefix (Expr)) & Deref_Suffix;
                  else
                     return Deref_Suffix;
                  end if;
               end Explicit_Dereference;

            when N_Expanded_Name
               | N_Selected_Component
            =>
               if Take_Prefix then
                  return
                    Expr_Name (Prefix (Expr)) & "." &
                    Expr_Name (Selector_Name (Expr));
               else
                  return "." & Expr_Name (Selector_Name (Expr));
               end if;

            when N_If_Expression =>
               declare
                  Cond_Expr : constant Node_Id := First (Expressions (Expr));
                  Then_Expr : constant Node_Id := Next (Cond_Expr);
                  Else_Expr : constant Node_Id := Next (Then_Expr);
               begin
                  return
                    "if " & Expr_Name (Cond_Expr) & " then "
                      & Expr_Name (Then_Expr) & " else "
                      & Expr_Name (Else_Expr);
               end;

            when N_Qualified_Expression =>
               declare
                  Mark : constant String :=
                           Expr_Name
                             (Subtype_Mark (Expr), Expand_Type => False);
                  Str  : constant String := Expr_Name (Expression (Expr));
               begin
                  if Str (Str'First) = '(' and then Str (Str'Last) = ')' then
                     return Mark & "'" & Str;
                  else
                     return Mark & "'(" & Str & ")";
                  end if;
               end;

            when N_Expression_With_Actions =>
               return Expr_Name (Expression (Expr));

            when N_Raise_Constraint_Error =>
               if Present (Condition (Expr)) then
                  return
                    "[constraint_error when "
                      & Expr_Name (Condition (Expr)) & "]";
               else
                  return "[constraint_error]";
               end if;

            when N_Raise_Program_Error =>
               if Present (Condition (Expr)) then
                  return
                    "[program_error when "
                      & Expr_Name (Condition (Expr)) & "]";
               else
                  return "[program_error]";
               end if;

            when N_Raise_Storage_Error =>
               if Present (Condition (Expr)) then
                  return
                    "[storage_error when "
                      & Expr_Name (Condition (Expr)) & "]";
               else
                  return "[storage_error]";
               end if;

            when N_Range =>
               return
                 Expr_Name (Low_Bound (Expr)) & ".." &
                 Expr_Name (High_Bound (Expr));

            when N_Slice =>
               return
                 Expr_Name (Prefix (Expr)) & " (" &
                 Expr_Name (Discrete_Range (Expr)) & ")";

            when N_And_Then =>
               return
                 Expr_Name (Left_Opnd (Expr)) & " and then " &
                 Expr_Name (Right_Opnd (Expr));

            when N_In =>
               return
                 Expr_Name (Left_Opnd (Expr)) & " in " &
                 Expr_Name (Right_Opnd (Expr));

            when N_Not_In =>
               return
                 Expr_Name (Left_Opnd (Expr)) & " not in " &
                 Expr_Name (Right_Opnd (Expr));

            when N_Or_Else =>
               return
                 Expr_Name (Left_Opnd (Expr)) & " or else " &
                 Expr_Name (Right_Opnd (Expr));

            when N_Op_And =>
               return
                 Expr_Name (Left_Opnd (Expr)) & " and " &
                 Expr_Name (Right_Opnd (Expr));

            when N_Op_Or =>
               return
                 Expr_Name (Left_Opnd (Expr)) & " or " &
                 Expr_Name (Right_Opnd (Expr));

            when N_Op_Xor =>
               return
                 Expr_Name (Left_Opnd (Expr)) & " xor " &
                 Expr_Name (Right_Opnd (Expr));

            when N_Op_Eq =>
               return
                 Expr_Name (Left_Opnd (Expr)) & " = " &
                 Expr_Name (Right_Opnd (Expr));

            when N_Op_Ne =>
               return
                 Expr_Name (Left_Opnd (Expr)) & " /= " &
                 Expr_Name (Right_Opnd (Expr));

            when N_Op_Lt =>
               return
                 Expr_Name (Left_Opnd (Expr)) & " < " &
                 Expr_Name (Right_Opnd (Expr));

            when N_Op_Le =>
               return
                 Expr_Name (Left_Opnd (Expr)) & " <= " &
                 Expr_Name (Right_Opnd (Expr));

            when N_Op_Gt =>
               return
                 Expr_Name (Left_Opnd (Expr)) & " > " &
                 Expr_Name (Right_Opnd (Expr));

            when N_Op_Ge =>
               return
                 Expr_Name (Left_Opnd (Expr)) & " >= " &
                 Expr_Name (Right_Opnd (Expr));

            when N_Op_Add =>
               return
                 Expr_Name (Left_Opnd (Expr)) & " + " &
                 Expr_Name (Right_Opnd (Expr));

            when N_Op_Subtract =>
               return
                 Expr_Name (Left_Opnd (Expr)) & " - " &
                 Expr_Name (Right_Opnd (Expr));

            when N_Op_Multiply =>
               return
                 Expr_Name (Left_Opnd (Expr)) & " * " &
                 Expr_Name (Right_Opnd (Expr));

            when N_Op_Divide =>
               return
                 Expr_Name (Left_Opnd (Expr)) & " / " &
                 Expr_Name (Right_Opnd (Expr));

            when N_Op_Mod =>
               return
                 Expr_Name (Left_Opnd (Expr)) & " mod " &
                 Expr_Name (Right_Opnd (Expr));

            when N_Op_Rem =>
               return
                 Expr_Name (Left_Opnd (Expr)) & " rem " &
                 Expr_Name (Right_Opnd (Expr));

            when N_Op_Expon =>
               return
                 Expr_Name (Left_Opnd (Expr)) & " ** " &
                 Expr_Name (Right_Opnd (Expr));

            when N_Op_Shift_Left =>
               return
                 Expr_Name (Left_Opnd (Expr)) & " << " &
                 Expr_Name (Right_Opnd (Expr));

            when N_Op_Shift_Right | N_Op_Shift_Right_Arithmetic =>
               return
                 Expr_Name (Left_Opnd (Expr)) & " >> " &
                 Expr_Name (Right_Opnd (Expr));

            when N_Op_Concat =>
               return
                 Expr_Name (Left_Opnd (Expr)) & " & " &
                 Expr_Name (Right_Opnd (Expr));

            when N_Op_Plus =>
               return "+" & Expr_Name (Right_Opnd (Expr));

            when N_Op_Minus =>
               return "-" & Expr_Name (Right_Opnd (Expr));

            when N_Op_Abs =>
               return "abs " & Expr_Name (Right_Opnd (Expr));

            when N_Op_Not =>
               return "not (" & Expr_Name (Right_Opnd (Expr)) & ")";

            when N_Type_Conversion =>

               --  Most conversions are not very interesting (used inside
               --  expanded checks to convert to larger ranges), so skip them.

               return Expr_Name (Expression (Expr));

            when N_Unchecked_Type_Conversion =>

               --  Only keep the type conversion in complex cases

               if not Is_Scalar_Type (Etype (Expr))
                 or else not Is_Scalar_Type (Etype (Expression (Expr)))
                 or else Is_Modular_Integer_Type (Etype (Expr)) /=
                           Is_Modular_Integer_Type (Etype (Expression (Expr)))
               then
                  return Expr_Name (Subtype_Mark (Expr)) &
                    "(" & Expr_Name (Expression (Expr)) & ")";
               else
                  return Expr_Name (Expression (Expr));
               end if;

            when N_Indexed_Component =>
               if Take_Prefix then
                  return
                    Expr_Name (Prefix (Expr))
                      & " (" & List_Name (Expressions (Expr)) & ')';
               else
                  return List_Name (Expressions (Expr));
               end if;

            when N_Function_Call =>

               --  If Default = "", it means we're expanding the name of
               --  a gnat temporary (and not really a function call), so add
               --  parentheses around function call to mark it specially.

               if Default = "" then
                  if Present (Parameter_Associations (Expr)) then
                     return '('
                       & Expr_Name (Name (Expr))
                       & " ("
                       & List_Name (Parameter_Associations (Expr))
                       & "))";
                  else
                     return '(' & Expr_Name (Name (Expr)) & ')';
                  end if;
               elsif Present (Parameter_Associations (Expr)) then
                  return
                    Expr_Name (Name (Expr))
                      & " (" & List_Name (Parameter_Associations (Expr)) & ')';
               else
                  return Expr_Name (Name (Expr));
               end if;

            when N_Null =>
               return "null";

            when N_Case_Expression
               | N_Delta_Aggregate
               | N_External_Initializer
               | N_Interpolated_String_Literal
               | N_Op_Rotate_Left
               | N_Op_Rotate_Right
               | N_Operator_Symbol
               | N_Procedure_Call_Statement
               | N_Quantified_Expression
               | N_Raise_Expression
               | N_Reference
               | N_Target_Name
            =>
               return "...";
         end case;
      end Expr_Name;

      -----------------------
      -- Count_Parentheses --
      -----------------------

      function Count_Parentheses (S : String; C : Character) return Natural is

         procedure Next_Char (Count : in out Natural; C, D, Ch : Character);
         --  Process next character Ch and update the number Count of C
         --  characters to add for correct parenthesizing, where D is the
         --  opposite parenthesis.

         ---------------
         -- Next_Char --
         ---------------

         procedure Next_Char (Count : in out Natural; C, D, Ch : Character) is
         begin
            if Ch = D then
               Count := Count + 1;
            elsif Ch = C and then Count > 0 then
               Count := Count - 1;
            end if;
         end Next_Char;

         --  Local variables

         Count : Natural := 0;

      --  Start of processing for Count_Parentheses

      begin
         if C = '(' then
            for Ch of reverse S loop
               Next_Char (Count, C, ')', Ch);
            end loop;
         else
            for Ch of S loop
               Next_Char (Count, C, '(', Ch);
            end loop;
         end if;

         return Count;
      end Count_Parentheses;

      ---------------------
      -- Fix_Parentheses --
      ---------------------

      function Fix_Parentheses (S : String) return String is
         Count_Open  : constant Natural := Count_Parentheses (S, '(');
         Count_Close : constant Natural := Count_Parentheses (S, ')');
      begin
         return (1 .. Count_Open => '(') & S & (1 .. Count_Close => ')');
      end Fix_Parentheses;

      --  Local variables

      Left, Right : Source_Ptr;

   --  Start of processing for Expression_Image

   begin
      --  Since this is an expression pretty-printer, it should not be called
      --  for anything but an expression. However, currently CodePeer calls
      --  it for defining identifiers. This should be fixed in the CodePeer
      --  itself, but for now simply return the default (if present) or print
      --  name of the defining identifier.

      if Nkind (Expr) = N_Defining_Identifier then
         pragma Assert (CodePeer_Mode);
         if Comes_From_Source (Expr)
           or else Opt.Debug_Generated_Code
         then
            if Default = "" then
               declare
                  Nam : constant Name_Id := Chars (Expr);
                  Buf : Bounded_String
                    (Max_Length => Natural (Length_Of_Name (Nam)));
               begin
                  Adjust_Name_Case (Buf, Sloc (Expr));
                  Append (Buf, Nam);
                  return To_String (Buf);
               end;
            else
               return Default;
            end if;
         else
            declare
               S : constant String :=
                 Ident_Image
                   (Expr => Expr, Orig_Expr => Expr, Expand_Type => True);
            begin
               if S = "..." then
                  return Default;
               else
                  return S;
               end if;
            end;
         end if;
      else
         pragma Assert (Nkind (Expr) in N_Subexpr);
      end if;

      --  ??? The following should be primarily needed for CodePeer

      if not Comes_From_Source (Expr)
        or else Opt.Debug_Generated_Code
      then
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

      --  Reach to the underlying expression for an expression-with-actions

      if Nkind (Expr) = N_Expression_With_Actions then
         return Expression_Image (Expression (Expr), Default);
      end if;

      --  Compute left (start) and right (end) slocs for the expression

      Left  := First_Sloc (Expr);
      Right := Last_Sloc (Expr);

      if Left > Right then
         return Default;
      end if;

      declare
         Scn : Source_Ptr := Left;
         Src : constant not null Source_Buffer_Ptr :=
           Source_Text (Get_Source_File_Index (Scn));

         Threshold        : constant := 256;
         Buffer           : String (1 .. Natural (Right - Left + 1));
         Index            : Natural := 0;
         Skipping_Comment : Boolean := False;
         Underscore       : Boolean := False;
      begin
         while Scn <= Right loop
            case Src (Scn) is

               --  Give up on non ASCII characters

               when Character'Val (128) .. Character'Last =>
                  Index := 0;
                  exit;

               when ' '
                  | ASCII.HT
               =>
                  if not Skipping_Comment and then not Underscore then
                     Underscore := True;
                     Index := Index + 1;
                     Buffer (Index) := ' ';
                  end if;

               --  CR/LF/FF is the end of any comment

               when ASCII.CR
                  | ASCII.FF
                  | ASCII.LF
               =>
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

            --  Give up on too long strings

            if Index >= Threshold then
               return Buffer (1 .. Index) & "...";
            end if;

            Scn := Scn + 1;
         end loop;

         return Fix_Parentheses (Buffer (1 .. Index));
      end;
   end Expression_Image;

end Pprint;

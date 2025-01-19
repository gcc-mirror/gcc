------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                S T Y L E                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2025, Free Software Foundation, Inc.         --
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
with Casing;         use Casing;
with Csets;          use Csets;
with Einfo;          use Einfo;
with Einfo.Entities; use Einfo.Entities;
with Einfo.Utils;    use Einfo.Utils;
with Errout;         use Errout;
with Namet;          use Namet;
with Nlists;         use Nlists;
with Opt;            use Opt;
with Sinfo;          use Sinfo;
with Sinfo.Nodes;    use Sinfo.Nodes;
with Sinput;         use Sinput;
with Snames;         use Snames;
with Stylesw;        use Stylesw;

package body Style is

   -----------------------
   -- Body_With_No_Spec --
   -----------------------

   --  If the check specs mode (-gnatys) is set, then all subprograms must
   --  have specs unless they are parameterless procedures at the library
   --  level (i.e. they are possible main programs).

   procedure Body_With_No_Spec (N : Node_Id) is
   begin
      if Style_Check_Specs then
         if Nkind (Parent (N)) = N_Compilation_Unit then
            declare
               Spec  : constant Node_Id := Specification (N);
               Defnm : constant Node_Id := Defining_Unit_Name (Spec);

            begin
               if Nkind (Spec) = N_Procedure_Specification
                 and then Nkind (Defnm) = N_Defining_Identifier
                 and then No (First_Formal (Defnm))
               then
                  return;
               end if;
            end;
         end if;

         Error_Msg_N ("(style) subprogram body has no previous spec?s?", N);
      end if;
   end Body_With_No_Spec;

   ---------------------------------
   -- Check_Array_Attribute_Index --
   ---------------------------------

   procedure Check_Array_Attribute_Index
     (N  : Node_Id;
      E1 : Node_Id;
      D  : Int)
   is
   begin
      if Style_Check_Array_Attribute_Index then
         if D = 1 and then Present (E1) then
            Error_Msg_N -- CODEFIX
              ("(style) index number not allowed for one dimensional array?A?",
               E1);
         elsif D > 1 and then No (E1) then
            Error_Msg_N -- CODEFIX
              ("(style) index number required for multi-dimensional array?A?",
               N);
         end if;
      end if;
   end Check_Array_Attribute_Index;

   ----------------------------
   -- Check_Boolean_Operator --
   ----------------------------

   procedure Check_Boolean_Operator (Node : Node_Id) is

      function OK_Boolean_Operand (N : Node_Id) return Boolean;
      --  Returns True for simple variable, or "not X1" or "X1 and X2" or
      --  "X1 or X2" where X1, X2 are recursively OK_Boolean_Operand's.

      ------------------------
      -- OK_Boolean_Operand --
      ------------------------

      function OK_Boolean_Operand (N : Node_Id) return Boolean is
      begin
         if Nkind (N) in N_Identifier | N_Expanded_Name then
            return True;

         elsif Nkind (N) = N_Op_Not then
            return OK_Boolean_Operand (Original_Node (Right_Opnd (N)));

         elsif Nkind (N) in N_Op_And | N_Op_Or then
            return OK_Boolean_Operand (Original_Node (Left_Opnd (N)))
                     and then
                   OK_Boolean_Operand (Original_Node (Right_Opnd (N)));

         else
            return False;
         end if;
      end OK_Boolean_Operand;

   --  Start of processing for Check_Boolean_Operator

   begin
      if Style_Check_Boolean_And_Or
        and then Comes_From_Source (Node)
      then
         declare
            Orig : constant Node_Id := Original_Node (Node);

         begin
            if Nkind (Orig) in N_Op_And | N_Op_Or then
               declare
                  L : constant Node_Id := Original_Node (Left_Opnd  (Orig));
                  R : constant Node_Id := Original_Node (Right_Opnd (Orig));

               begin
                  --  First OK case, simple boolean constants/identifiers

                  if OK_Boolean_Operand (L)
                       and then
                     OK_Boolean_Operand (R)
                  then
                     return;

                  --  Second OK case, modular types

                  elsif Is_Modular_Integer_Type (Etype (Node)) then
                     return;

                  --  Third OK case, array types

                  elsif Is_Array_Type (Etype (Node)) then
                     return;

                  --  Otherwise we have an error

                  elsif Nkind (Orig) = N_Op_And then
                     Error_Msg -- CODEFIX
                       ("(style) `AND THEN` required?B?", Sloc (Orig), Orig);
                  else
                     Error_Msg -- CODEFIX
                       ("(style) `OR ELSE` required?B?", Sloc (Orig), Orig);
                  end if;
               end;
            end if;
         end;
      end if;
   end Check_Boolean_Operator;

   ----------------------
   -- Check_Identifier --
   ----------------------

   --  In check references mode (-gnatyr), identifier uses must be cased
   --  the same way as the corresponding identifier declaration. If standard
   --  references are checked (-gnatyn), then identifiers from Standard must
   --  be cased as in the Reference Manual.

   procedure Check_Identifier
     (Ref : Node_Or_Entity_Id;
      Def : Node_Or_Entity_Id)
   is
      Sref : Source_Ptr := Sloc (Ref);
      Sdef : Source_Ptr := Sloc (Def);
      Tref : Source_Buffer_Ptr;
      Tdef : Source_Buffer_Ptr;
      Nlen : Nat;
      Cas  : Casing_Type;

   begin
      --  If reference does not come from source, nothing to check

      if not Comes_From_Source (Ref) then
         return;

      --  If previous error on either node/entity, ignore

      elsif Error_Posted (Ref) or else Error_Posted (Def) then
         return;

      --  Case of definition comes from source, or a record component whose
      --  Original_Record_Component comes from source.

      elsif Comes_From_Source (Def) or else
        (Ekind (Def) in Record_Field_Kind
           and then Present (Original_Record_Component (Def))
           and then Comes_From_Source (Original_Record_Component (Def)))
      then

         --  Check same casing if we are checking references

         if Style_Check_References then
            Tref := Source_Text (Get_Source_File_Index (Sref));
            Tdef := Source_Text (Get_Source_File_Index (Sdef));

            --  Ignore case of operator names. This also catches the case
            --  where one is an operator and the other is not. This is a
            --  phenomenon from rewriting of operators as functions, and is
            --  to be ignored.

            if Tref (Sref) = '"' or else Tdef (Sdef) = '"' then
               return;

            else
               loop
                  --  If end of identifiers, all done. Note that they are the
                  --  same length.

                  pragma Assert
                    (Identifier_Char (Tref (Sref)) =
                     Identifier_Char (Tdef (Sdef)));

                  if not Identifier_Char (Tref (Sref)) then
                     return;
                  end if;

                  --  Case mismatch

                  if Tref (Sref) /= Tdef (Sdef) then
                     Error_Msg_Node_1 := Def;
                     Error_Msg_Sloc := Sloc (Def);
                     Error_Msg -- CODEFIX
                       ("(style) bad casing of & declared#?r?", Sref, Ref);
                     return;
                  end if;

                  Sref := Sref + 1;
                  Sdef := Sdef + 1;
               end loop;

               pragma Assert (False);
            end if;
         end if;

      --  Case of definition in package Standard

      elsif Sdef = Standard_Location
              or else
            Sdef = Standard_ASCII_Location
      then
         --  Check case of identifiers in Standard

         if Style_Check_Standard then
            Tref := Source_Text (Get_Source_File_Index (Sref));

            --  Ignore operators

            if Tref (Sref) = '"' then
               null;

            --  Otherwise determine required casing of Standard entity

            else
               --  ASCII is all upper case

               if Chars (Ref) = Name_ASCII then
                  Cas := All_Upper_Case;

               --  Special handling for names in package ASCII

               elsif Sdef = Standard_ASCII_Location then
                  declare
                     Nam : constant String := Get_Name_String (Chars (Def));

                  begin
                     --  Bar is mixed case

                     if Nam = "bar" then
                        Cas := Mixed_Case;

                     --  All names longer than 4 characters are mixed case

                     elsif Nam'Length > 4 then
                        Cas := Mixed_Case;

                     --  All names shorter than 4 characters (other than Bar,
                     --  which we already tested for specially) are Upper case.

                     else
                        Cas := All_Upper_Case;
                     end if;
                  end;

               --  All other entities are in mixed case

               else
                  Cas := Mixed_Case;
               end if;

               Nlen := Length_Of_Name (Chars (Ref));

               --  Now check if we have the right casing

               if Determine_Casing
                    (Tref (Sref .. Sref + Source_Ptr (Nlen) - 1)) = Cas
               then
                  null;
               else
                  Name_Len := Integer (Nlen);
                  Name_Buffer (1 .. Name_Len) :=
                    String (Tref (Sref .. Sref + Source_Ptr (Nlen) - 1));
                  Set_Casing (Cas);
                  Error_Msg_Name_1 := Name_Enter;
                  Error_Msg_N -- CODEFIX
                    ("(style) bad casing of %% declared in Standard?n?", Ref);
               end if;
            end if;
         end if;
      end if;
   end Check_Identifier;

   ----------------------------------
   -- Check_Xtra_Parens_Precedence --
   ----------------------------------

   procedure Check_Xtra_Parens_Precedence (N : Node_Id) is
   begin
      if Style_Check_Xtra_Parens_Precedence
        and then
          Paren_Count (N) >
            (if Nkind (N) in N_Case_Expression
                           | N_Expression_With_Actions
                           | N_If_Expression
                           | N_Quantified_Expression
                           | N_Raise_Expression
             then 1
             else 0)
      then
         Error_Msg -- CODEFIX
           ("(style) redundant parentheses?z?", First_Sloc (N), N);
      end if;
   end Check_Xtra_Parens_Precedence;

   ------------------------
   -- Missing_Overriding --
   ------------------------

   procedure Missing_Overriding (N : Node_Id; E : Entity_Id) is
      Nod : Node_Id;

   begin
      --  Perform the check on source subprograms and on subprogram instances,
      --  because these can be primitives of untagged types. Note that such
      --  indicators were introduced in Ada 2005. We apply Comes_From_Source
      --  to Original_Node to catch the case of a procedure body declared with
      --  "is null" that has been rewritten as a normal empty body.
      --  We do not emit a warning on an inherited operation that comes from
      --  a type derivation.

      if Style_Check_Missing_Overriding
        and then (Comes_From_Source (Original_Node (N))
                   or else Is_Generic_Instance (E))
        and then Ada_Version_Explicit >= Ada_2005
        and then Present (Parent (E))
        and then Nkind (Parent (E)) /= N_Full_Type_Declaration
      then
         --  If the subprogram is an instantiation,  its declaration appears
         --  within a wrapper package that precedes the instance node. Place
         --  warning on the node to avoid references to the original generic.

         if Nkind (N) = N_Subprogram_Declaration
           and then Is_Generic_Instance (E)
         then
            Nod := Next (Parent (Parent (List_Containing (N))));
         else
            Nod := N;
         end if;

         if Nkind (N) = N_Subprogram_Body then
            Error_Msg_NE -- CODEFIX
              ("(style) missing OVERRIDING indicator in body of&?O?", N, E);

         elsif Nkind (N) = N_Abstract_Subprogram_Declaration then
            Error_Msg_NE -- CODEFIX
              ("(style) missing OVERRIDING indicator in declaration of&?O?",
                Specification (N), E);

         else
            Error_Msg_NE -- CODEFIX
              ("(style) missing OVERRIDING indicator in declaration of&?O?",
               Nod, E);
         end if;
      end if;
   end Missing_Overriding;

   -----------------------------------
   -- Subprogram_Not_In_Alpha_Order --
   -----------------------------------

   procedure Subprogram_Not_In_Alpha_Order (Name : Node_Id) is
   begin
      if Style_Check_Order_Subprograms then
         Error_Msg_N -- CODEFIX
           ("(style) subprogram body& not in alphabetical order?o?", Name);
      end if;
   end Subprogram_Not_In_Alpha_Order;
end Style;

------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S I N F O . C N                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2024, Free Software Foundation, Inc.         --
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

--  This child package of Sinfo contains some routines that permit in place
--  alteration of existing tree nodes by changing the value in the Nkind
--  field. Since Nkind functions logically in a manner similar to a variant
--  record discriminant part, such alterations cannot be permitted in a
--  general manner, but in some specific cases, the fields of related nodes
--  have been deliberately layed out in a manner that permits such alteration.

with Atree;          use Atree;
with Snames;         use Snames;
with Sinfo.Nodes;    use Sinfo.Nodes;
with Sinfo.Utils;    use Sinfo.Utils;

package body Sinfo.CN is

   ------------------------------------------------------------
   -- Change_Character_Literal_To_Defining_Character_Literal --
   ------------------------------------------------------------

   procedure Change_Character_Literal_To_Defining_Character_Literal
     (N : Node_Id)
   is
   begin
      Reinit_Field_To_Zero (N, F_Char_Literal_Value);
      Extend_Node (N);
   end Change_Character_Literal_To_Defining_Character_Literal;

   ------------------------------------
   -- Change_Conversion_To_Unchecked --
   ------------------------------------

   procedure Change_Conversion_To_Unchecked (N : Node_Id) is
   begin
      Set_Do_Overflow_Check (N, False);
      Set_Do_Length_Check (N, False);
      Mutate_Nkind (N, N_Unchecked_Type_Conversion);
   end Change_Conversion_To_Unchecked;

   ----------------------------------------------
   -- Change_Identifier_To_Defining_Identifier --
   ----------------------------------------------

   procedure Change_Identifier_To_Defining_Identifier (N : Node_Id) is
   begin
      Extend_Node (N);
   end Change_Identifier_To_Defining_Identifier;

   ---------------------------------------------
   -- Change_Name_To_Procedure_Call_Statement --
   ---------------------------------------------

   procedure Change_Name_To_Procedure_Call_Statement (N : Node_Id) is
   begin
      --  Case of Indexed component, which is a procedure call with arguments

      if Nkind (N) = N_Indexed_Component then
         declare
            Prefix_Node : constant Node_Id := Prefix (N);
            Exprs_Node  : constant List_Id := Expressions (N);

         begin
            Change_Node (N, N_Procedure_Call_Statement);
            Set_Name (N, Prefix_Node);
            Set_Parameter_Associations (N, Exprs_Node);
         end;

      --  Case of function call node, which is a really a procedure call

      elsif Nkind (N) = N_Function_Call then
         declare
            Fname_Node  : constant Node_Id := Name (N);
            Params_List : constant List_Id := Parameter_Associations (N);

         begin
            Change_Node (N, N_Procedure_Call_Statement);
            Set_Name (N, Fname_Node);
            Set_Parameter_Associations (N, Params_List);
         end;

      --  Case of call to attribute that denotes a procedure. Here we just
      --  leave the attribute reference unchanged.

      elsif Nkind (N) = N_Attribute_Reference
        and then Is_Procedure_Attribute_Name (Attribute_Name (N))
      then
         null;

      --  All other cases of names are parameterless procedure calls

      else
         declare
            Name_Node : constant Node_Id := Relocate_Node (N);
         begin
            Change_Node (N, N_Procedure_Call_Statement);
            Set_Name (N, Name_Node);
         end;
      end if;
   end Change_Name_To_Procedure_Call_Statement;

   --------------------------------------------------------
   -- Change_Operator_Symbol_To_Defining_Operator_Symbol --
   --------------------------------------------------------

   procedure Change_Operator_Symbol_To_Defining_Operator_Symbol
     (N : Node_Id)
   is
   begin
      Reinit_Field_To_Zero (N, F_Strval);
      Extend_Node (N);
   end Change_Operator_Symbol_To_Defining_Operator_Symbol;

   ----------------------------------------------
   -- Change_Operator_Symbol_To_String_Literal --
   ----------------------------------------------

   procedure Change_Operator_Symbol_To_String_Literal (N : Node_Id) is
   begin
      Reinit_Field_To_Zero (N, F_Chars);
      Set_Entity (N, Empty);
      Mutate_Nkind (N, N_String_Literal);
   end Change_Operator_Symbol_To_String_Literal;

   ------------------------------------------------
   -- Change_Selected_Component_To_Expanded_Name --
   ------------------------------------------------

   procedure Change_Selected_Component_To_Expanded_Name (N : Node_Id) is
   begin
      Mutate_Nkind (N, N_Expanded_Name);
      Set_Chars (N, Chars (Selector_Name (N)));
   end Change_Selected_Component_To_Expanded_Name;

end Sinfo.CN;

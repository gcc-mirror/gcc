------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S I N F O . C N                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2008, Free Software Foundation, Inc.         --
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

with Atree; use Atree;

package body Sinfo.CN is

   use Atree.Unchecked_Access;
   --  This package is one of the few packages which is allowed to make direct
   --  references to tree nodes (since it is in the business of providing a
   --  higher level of tree access which other clients are expected to use and
   --  which implements checks).

   ------------------------------------------------------------
   -- Change_Character_Literal_To_Defining_Character_Literal --
   ------------------------------------------------------------

   procedure Change_Character_Literal_To_Defining_Character_Literal
     (N : in out Node_Id)
   is
   begin
      Set_Nkind (N, N_Defining_Character_Literal);
      N := Extend_Node (N);
   end Change_Character_Literal_To_Defining_Character_Literal;

   ------------------------------------
   -- Change_Conversion_To_Unchecked --
   ------------------------------------

   procedure Change_Conversion_To_Unchecked (N : Node_Id) is
   begin
      Set_Do_Overflow_Check (N, False);
      Set_Do_Tag_Check (N, False);
      Set_Do_Length_Check (N, False);
      Set_Nkind (N, N_Unchecked_Type_Conversion);
   end Change_Conversion_To_Unchecked;

   ----------------------------------------------
   -- Change_Identifier_To_Defining_Identifier --
   ----------------------------------------------

   procedure Change_Identifier_To_Defining_Identifier (N : in out Node_Id) is
   begin
      Set_Nkind (N, N_Defining_Identifier);
      N := Extend_Node (N);
   end Change_Identifier_To_Defining_Identifier;

   --------------------------------------------------------
   -- Change_Operator_Symbol_To_Defining_Operator_Symbol --
   --------------------------------------------------------

   procedure Change_Operator_Symbol_To_Defining_Operator_Symbol
     (N : in out Node_Id)
   is
   begin
      Set_Nkind (N, N_Defining_Operator_Symbol);
      Set_Node2 (N, Empty); -- Clear unused Str2 field
      N := Extend_Node (N);
   end Change_Operator_Symbol_To_Defining_Operator_Symbol;

   ----------------------------------------------
   -- Change_Operator_Symbol_To_String_Literal --
   ----------------------------------------------

   procedure Change_Operator_Symbol_To_String_Literal (N : Node_Id) is
   begin
      Set_Nkind (N, N_String_Literal);
      Set_Node1 (N, Empty); -- clear Name1 field
   end Change_Operator_Symbol_To_String_Literal;

   ------------------------------------------------
   -- Change_Selected_Component_To_Expanded_Name --
   ------------------------------------------------

   procedure Change_Selected_Component_To_Expanded_Name (N : Node_Id) is
   begin
      Set_Nkind (N, N_Expanded_Name);
      Set_Chars (N, Chars (Selector_Name (N)));
   end Change_Selected_Component_To_Expanded_Name;

end Sinfo.CN;

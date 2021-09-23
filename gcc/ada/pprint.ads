------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               P P R I N T                                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2008-2021, Free Software Foundation, Inc.         --
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

--  This package (pretty print) contains a routine for printing an expression
--  given its node in the syntax tree. Contrarily to the Sprint package, this
--  routine tries to obtain "pretty" output that can be used for e.g. error
--  messages.

with Types;  use Types;
with Urealp; use Urealp;

package Pprint is

   generic

      --  ??? The generic parameters should be removed.

      with function Real_Image (U : Ureal) return String;
      with function String_Image (S : String_Id) return String;
      with function Ident_Image (Expr        : Node_Id;
                                 Orig_Expr   : Node_Id;
                                 Expand_Type : Boolean) return String;
      --  Will be called for printing N_Identifier and N_Defining_Identifier
      --  nodes
      --  ??? Expand_Type argument should be removed

      Hide_Parameter_Blocks : Boolean := False;
      --  If true, then "Parameter_Block.Field_Name.all" is
      --  instead displayed as "Field_Name".

      Hide_Temp_Derefs : Boolean := False;
      --  If true, then "Foo.all" is instead displayed as "Foo"
      --  in the case where Foo is a compiler-generated constant
      --  initialized to Some_Captured_Value'Reference.

   function Expression_Image
     (Expr    : Node_Id;
      Default : String) return String;
   --  Given a Node for an expression, return a String that is meaningful for
   --  the programmer. If the expression comes from source, it is copied from
   --  there.
   --  Subexpressions outside of the maximum depth (3), the maximal number of
   --  accepted nodes (24), and the maximal number of list elements (3), are
   --  replaced by the default string.

end Pprint;

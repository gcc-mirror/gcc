------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ I M G V                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--            Copyright (C) 2000 Free Software Foundation, Inc.             --
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

--  Expand routines for Image, Value and Width attributes. These are the
--  attributes that make use of enumeration type image tables.

with Types; use Types;

package Exp_Imgv is

   procedure Build_Enumeration_Image_Tables (E : Entity_Id; N : Node_Id);
   --  Build the enumeration image tables for E, which is an enumeration
   --  base type. The node N is the point in the tree where the resulting
   --  declarations are to be inserted.
   --
   --    The form of the tables generated is as follows:
   --
   --      xxxS : string := "chars";
   --      xxxI : array (0 .. N) of Natural_8/16/32 := (1, n, .., n);
   --
   --    Here xxxS is a string obtained by concatenating all the names
   --    of the enumeration literals in sequence, representing any wide
   --    characters according to the current wide character encoding
   --    method, and with all letters forced to upper case.
   --
   --    The array xxxI is an array of ones origin indexes to the start
   --    of each name, with one extra entry at the end, which is the index
   --    to the character just past the end of the last literal, i.e. it is
   --    the length of xxxS + 1. The element type is the shortest of the
   --    possible types that will hold all the values.
   --
   --      For example, for the type
   --
   --         type x is (hello,'!',goodbye);
   --
   --      the generated tables would consist of
   --
   --          xxxS : String := "hello'!'goodbye";
   --          xxxI : array (0 .. 3) of Natural_8 := (1, 6, 9, 16);
   --
   --      Here Natural_8 is used since 16 < 2**(8-1)
   --
   --    If the entity E needs the tables constructing, the necessary
   --    declarations are constructed, and the fields Lit_Strings and
   --    Lit_Indexes of E are set to point to the corresponding entities.
   --    If no tables are needed (E is not a user defined enumeration
   --    root type, or pragma Discard_Names is in effect, then the
   --    declarations are not constructed, and the fields remain Empty.

   procedure Expand_Image_Attribute (N : Node_Id);
   --  This procedure is called from Exp_Attr to expand an occurrence
   --  of the attribute Image.

   procedure Expand_Value_Attribute (N : Node_Id);
   --  This procedure is called from Exp_Attr to expand an occurrence
   --  of the attribute Value.

   procedure Expand_Width_Attribute (N : Node_Id; Wide : Boolean);
   --  This procedure is called from Exp_Attr to expand an occurrence of
   --  the attributes Width (Wide = False) or Wide_Width (Wide = True).

end Exp_Imgv;

------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                         R E P I N F O - I N P U T                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2018-2025, Free Software Foundation, Inc.         --
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

--  This package provides an alternate way of populating the internal tables
--  of Repinfo from a JSON input rather than the binary blob of the tree file.
--  Note that this is an additive mechanism, i.e. nothing is destroyed in the
--  internal state of the unit when it is used.

--  The first step is to feed the unit with a JSON stream of a specified format
--  (see the spec of Repinfo for its description) by means of Read_JSON_Stream.
--  Then, for each entity whose representation information is present in the
--  JSON stream, the appropriate Get_JSON_* routines can be invoked to override
--  the eponymous fields of the entity in the tree.

package Repinfo.Input is

   function Get_JSON_Esize (Name : String) return Node_Ref_Or_Val;
   --  Returns the Esize value of the entity specified by Name, which is not
   --  the component of a record type, or else No_Uint if no representation
   --  information was supplied for the entity. Name is the full qualified name
   --  of the entity in lower case letters.

   function Get_JSON_RM_Size (Name : String) return Node_Ref_Or_Val;
   --  Likewise for the RM_Size

   function Get_JSON_Component_Size (Name : String) return Node_Ref_Or_Val;
   --  Likewise for the Component_Size of an array type

   function Get_JSON_Component_Bit_Offset
     (Name        : String;
      Record_Name : String) return Node_Ref_Or_Val;
   --  Returns the Component_Bit_Offset of the component specified by Name,
   --  which is declared in the record type specified by Record_Name, or else
   --  No_Uint if no representation information was supplied for the component.
   --  Name is the unqualified name of the component whereas Record_Name is the
   --  full qualified name of the record type, both in lower case letters.

   function Get_JSON_Esize
     (Name        : String;
      Record_Name : String) return Node_Ref_Or_Val;
   --  Likewise for the Esize

   Invalid_JSON_Stream : exception;
   --  Raised if a format error is detected in the JSON stream

   procedure Read_JSON_Stream (Text : Text_Buffer; File_Name : String);
   --  Reads a JSON stream and populates internal tables from it. File_Name is
   --  only used in error messages issued by the JSON parser.

end Repinfo.Input;

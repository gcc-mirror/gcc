------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                S Y S T E M . F I L E _ A T T R I B U T E S               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 2013-2020, Free Software Foundation, Inc.      --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package provides a binding to the GNAT file attribute query functions

with System.OS_Constants;
with System.Storage_Elements;

package System.File_Attributes is

   type File_Attributes is private;

   procedure Reset_Attributes (A : access File_Attributes);

   function Error_Attributes (A : access File_Attributes) return Integer;

   function File_Exists_Attr
     (N : System.Address;
      A : access File_Attributes) return Integer;

   function Is_Regular_File_Attr
     (N : System.Address;
      A : access File_Attributes) return Integer;

   function Is_Directory_Attr
     (N : System.Address;
      A : access File_Attributes) return Integer;

private
   package SOSC renames System.OS_Constants;

   type File_Attributes is new
     System.Storage_Elements.Storage_Array
       (1 .. SOSC.SIZEOF_struct_file_attributes);
   for File_Attributes'Alignment use Standard'Maximum_Alignment;

   pragma Import (C, Reset_Attributes,     "__gnat_reset_attributes");
   pragma Import (C, Error_Attributes,     "__gnat_error_attributes");
   pragma Import (C, File_Exists_Attr,     "__gnat_file_exists_attr");
   pragma Import (C, Is_Regular_File_Attr, "__gnat_is_regular_file_attr");
   pragma Import (C, Is_Directory_Attr,    "__gnat_is_directory_attr");

end System.File_Attributes;

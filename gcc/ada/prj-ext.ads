------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              P R J . E X T                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--             Copyright (C) 2000 Free Software Foundation, Inc.            --
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

--  Set, Get and cache External reference, to be used as External functions
--  in project files.

with Types; use Types;

package Prj.Ext is

   procedure Add
     (External_Name : String;
      Value         : String);
   --  Add an external reference (or modify an existing one).

   function Value_Of
     (External_Name : Name_Id;
      With_Default  : String_Id := No_String)
      return          String_Id;
   --  Get the value of an external reference, and cache it for future uses.

   function Check (Declaration : String) return Boolean;
   --  Check that an external declaration <external>=<value> is correct.
   --  If it is correct, the external reference is Added.

end Prj.Ext;

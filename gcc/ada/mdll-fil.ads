------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                            M D L L . F I L E S                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--          Copyright (C) 1992-2001 Free Software Foundation, Inc.          --
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
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

--  Simple services used by GNATDLL to deal with Filename extension

package MDLL.Fil is

   No_Ext : constant String := "";
   --  Used to mark the absence of an extension

   function Get_Ext (Filename : String) return String;
   --  Return extension of Filename

   function Is_Ali (Filename : String) return Boolean;
   --  Test if Filename is an Ada library file (.ali).

   function Is_Obj (Filename : String) return Boolean;
   --  Test if Filename is an object file (.o or .obj)

   function Ext_To
     (Filename : String;
      New_Ext  : String := No_Ext)
      return     String;
   --  Return Filename with the extension change to New_Ext

end MDLL.Fil;

------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              S Y M B O L S                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2003-2007, Free Software Foundation, Inc.         --
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

--  This is the default version of this package, used when the creation
--  of symbol files is not supported.

with Ada.Text_IO; use Ada.Text_IO;

package body Symbols is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Symbol_File   : String;
      Reference     : String;
      Symbol_Policy : Policy;
      Quiet         : Boolean;
      Version       : String;
      Success       : out Boolean)
   is
      pragma Unreferenced (Symbol_File);
      pragma Unreferenced (Reference);
      pragma Unreferenced (Symbol_Policy);
      pragma Unreferenced (Quiet);
      pragma Unreferenced (Version);
   begin
      Put_Line
        ("creation of symbol files are not supported on this platform");
      Success := False;
   end Initialize;

   ----------------
   -- Processing --
   ----------------

   package body Processing is

      -------------
      -- Process --
      -------------

      procedure Process
        (Object_File : String;
         Success     : out Boolean)
      is
         pragma Unreferenced (Object_File);
      begin
         Success := False;
      end Process;

   end Processing;

   --------------
   -- Finalize --
   --------------

   procedure Finalize
     (Quiet   : Boolean;
      Success : out Boolean)
   is
      pragma Unreferenced (Quiet);
   begin
      Success := False;
   end Finalize;

end Symbols;

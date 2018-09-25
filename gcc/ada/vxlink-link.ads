------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                          V X L I N K . L I N K                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2018, AdaCore                          --
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

pragma Ada_2012;

private with Ada.Strings.Unbounded;

package VxLink.Link is

   type VxLink_Linker is private;

   procedure Initialize
     (Linker : out VxLink_Linker);

   function Needs_CDtor (Linker : VxLink_Linker) return Boolean;

   function Partial_Object (Linker : VxLink_Linker) return String;

   function Namespace (Linker : VxLink_Linker) return String;

   procedure Do_Initial_Link
     (Linker : VxLink_Linker);

   procedure Do_Final_Link
     (Linker   : VxLink_Linker;
      Ctdt_Obj : String);

private

   use Ada.Strings.Unbounded;

   type VxLink_Linker is record
      Args_Leading  : Arguments_List;
      Args_Trailing : Arguments_List;
      Add_CDtors    : Boolean := True;
      Dest_Object   : Unbounded_String;
      Dest_Base     : Unbounded_String;
      Partial_Obj   : Unbounded_String;
   end record;

end VxLink.Link;

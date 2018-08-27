------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               V X L I N K                                --
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

--  See vxlink-main.adb for a description of the tool.
--
--  This package contains only common utility functions used by the other
--  child packages.

pragma Ada_2012;

with Ada.Containers.Indefinite_Vectors;

package VxLink is

   package Strings_List is new Ada.Containers.Indefinite_Vectors
     (Positive, String);

   subtype Arguments_List is Strings_List.Vector;

   procedure Set_Verbose (Value : Boolean);
   function Is_Verbose return Boolean;

   procedure Set_Error_State (Message : String);
   function Is_Error_State return Boolean;

   procedure Log_Info (S : String);
   procedure Log_Error (S : String);

   procedure Run (Arguments : Arguments_List);

   function Run (Arguments : Arguments_List) return String;

   function Gcc return String;
   --  Current toolchain's gcc command

   function Gxx return String;
   --  Current toolchain's g++ command

   function Nm return String;
   --  Current toolchain's nm command

   function Ends_With (Str, Suffix : String) return Boolean
   is (Str'Length >= Suffix'Length
       and then Str (Str'Last - Suffix'Length + 1 .. Str'Last) = Suffix);

end VxLink;

------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                          V X L I N K . B I N D                           --
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

private with Ada.Containers.Ordered_Sets;
private with Ada.Strings.Unbounded;

package VxLink.Bind is

   type VxLink_Binder is private;

   procedure Initialize
     (Binder      : out VxLink_Binder;
      Object_File : String);

   procedure Parse_Tag_File
     (Binder : in out VxLink_Binder;
      File   : String);

   procedure Emit_CTDT
     (Binder    : in out VxLink_Binder;
      Namespace : String);

   function CTDT_File (Binder : VxLink_Binder) return String;

private

   use Ada.Strings.Unbounded;

   type Symbol_Kind is (Sym_Ctor, Sym_Dtor, Sym_Other);

   type Symbol is record
      Name     : Unbounded_String;
      Cat      : Character;
      Internal : Boolean;
      Kind     : Symbol_Kind;
      Priority : Integer;
   end record;

   function "=" (S1, S2 : Symbol) return Boolean
   is (S1.Name = S2.Name and then S1.Cat = S2.Cat);

   function "<" (S1, S2 : Symbol) return Boolean
   is (if S1.Priority /= S2.Priority
       then S1.Priority < S2.Priority
       elsif S1.Name /= S2.Name
       then S1.Name < S2.Name
       else S1.Cat < S2.Cat);

   package Symbol_Sets is new Ada.Containers.Ordered_Sets
     (Symbol,
      "<" => "<",
      "=" => "=");

   type VxLink_Binder is record
      CTDT_File          : Unbounded_String;
      Constructors       : Symbol_Sets.Set;
      Destructors        : Symbol_Sets.Set;
      Module_Dtor_Needed : Boolean;
      EH_Frame_Needed    : Boolean;
      Tags_List          : Strings_List.Vector;
   end record;

end VxLink.Bind;

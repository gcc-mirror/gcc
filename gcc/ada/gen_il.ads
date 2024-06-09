------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                G E N _ I L                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2020-2024, Free Software Foundation, Inc.         --
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

pragma Warnings (Off); -- with clauses for children
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Streams.Stream_IO;
pragma Warnings (On);

package Gen_IL is -- generate intermediate language

   --  This package and children generates the main intermediate language used
   --  by the GNAT compiler, which is a decorated syntax tree.

   --  The generated Ada packages are:
   --
   --    Seinfo
   --    Sinfo.Nodes
   --    Einfo.Entities
   --    Nmake
   --    Seinfo_Tables
   --
   --  We also generate C code:
   --
   --    einfo.h
   --    sinfo.h
   --    snames.h
   --
   --  It is necessary to look at this generated code in order to understand
   --  the compiler. In addition, it is necessary to look at comments in the
   --  spec and body of Gen_IL.
   --
   --  Note that the Gen_IL "compiler" and the GNAT Ada compiler are separate
   --  programs, with no dependencies between them in either direction. That
   --  is, Gen_IL does not say "with" of GNAT units, and GNAT does not say
   --  "with Gen_IL". There are many things declared in Gen_IL and GNAT with
   --  the same name; these are typically related, but they are not the same
   --  thing.

   --  Misc declarations used throughout:

   type Root_Int is new Integer;
   function Image (X : Root_Int) return String;
   --  Without the extra blank. You can derive from Root_Int or the subtypes
   --  below, and inherit a convenient Image function that leaves out that
   --  blank.

   subtype Root_Nat is Root_Int range 0 .. Root_Int'Last;
   subtype Root_Pos is Root_Int range 1 .. Root_Int'Last;

   function Capitalize (S : String) return String;
   procedure Capitalize (S : in out String);
   --  Turns an identifier into Mixed_Case

   --  The following declares a minimal implementation of formatted output
   --  that is piggybacked on Ada.Streams.Stream_IO for bootstrap reasons.
   --  It uses LF as universal line terminator to make it host independent.

   type Sink is record
      File     : Ada.Streams.Stream_IO.File_Type;
      Indent   : Natural;
      New_Line : Boolean;
   end record;

   procedure Create_File (Buffer : in out Sink; Name : String);

   procedure Increase_Indent (Buffer : in out Sink; Amount : Natural);

   procedure Decrease_Indent (Buffer : in out Sink; Amount : Natural);

   procedure Put (Buffer : in out Sink; Item : String);

   LF : constant String := "" & ASCII.LF;

end Gen_IL;

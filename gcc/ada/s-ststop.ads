------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--              S Y S T E M . S T R I N G S . S T R E A M _ O P S           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 2008, Free Software Foundation, Inc.           --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package provides subprogram implementations of stream attributes for
--  the following types:
--     Ada.String
--     Ada.Wide_String
--     Ada.Wide_Wide_String
--
--  The compiler will generate references to the subprograms in this package
--  when expanding stream attributes for the above mentioned types. Example:
--
--     String'Output (Some_Stream, Some_String);
--
--  will be expanded into:
--
--     String_Output (Some_Stream, Some_String);
--       or
--     String_Output_Blk_IO (Some_Stream, Some_String);

pragma Warnings (Off);
pragma Compiler_Unit;
pragma Warnings (On);

with Ada.Streams;

package System.Strings.Stream_Ops is

   ------------------------------
   -- String stream operations --
   ------------------------------

   function String_Input
     (Strm : access Ada.Streams.Root_Stream_Type'Class)
      return String;

   function String_Input_Blk_IO
     (Strm : access Ada.Streams.Root_Stream_Type'Class)
      return String;

   procedure String_Output
     (Strm : access Ada.Streams.Root_Stream_Type'Class;
      Item : String);

   procedure String_Output_Blk_IO
     (Strm : access Ada.Streams.Root_Stream_Type'Class;
      Item : String);

   procedure String_Read
     (Strm : access Ada.Streams.Root_Stream_Type'Class;
      Item : out String);

   procedure String_Read_Blk_IO
     (Strm : access Ada.Streams.Root_Stream_Type'Class;
      Item : out String);

   procedure String_Write
     (Strm : access Ada.Streams.Root_Stream_Type'Class;
      Item : String);

   procedure String_Write_Blk_IO
     (Strm : access Ada.Streams.Root_Stream_Type'Class;
      Item : String);

   -----------------------------------
   -- Wide_String stream operations --
   -----------------------------------

   function Wide_String_Input
     (Strm : access Ada.Streams.Root_Stream_Type'Class)
      return Wide_String;

   function Wide_String_Input_Blk_IO
     (Strm : access Ada.Streams.Root_Stream_Type'Class)
      return Wide_String;

   procedure Wide_String_Output
     (Strm : access Ada.Streams.Root_Stream_Type'Class;
      Item : Wide_String);

   procedure Wide_String_Output_Blk_IO
     (Strm : access Ada.Streams.Root_Stream_Type'Class;
      Item : Wide_String);

   procedure Wide_String_Read
     (Strm : access Ada.Streams.Root_Stream_Type'Class;
      Item : out Wide_String);

   procedure Wide_String_Read_Blk_IO
     (Strm : access Ada.Streams.Root_Stream_Type'Class;
      Item : out Wide_String);

   procedure Wide_String_Write
     (Strm : access Ada.Streams.Root_Stream_Type'Class;
      Item : Wide_String);

   procedure Wide_String_Write_Blk_IO
     (Strm : access Ada.Streams.Root_Stream_Type'Class;
      Item : Wide_String);

   ----------------------------------------
   -- Wide_Wide_String stream operations --
   ----------------------------------------

   function Wide_Wide_String_Input
     (Strm : access Ada.Streams.Root_Stream_Type'Class)
      return Wide_Wide_String;

   function Wide_Wide_String_Input_Blk_IO
     (Strm : access Ada.Streams.Root_Stream_Type'Class)
      return Wide_Wide_String;

   procedure Wide_Wide_String_Output
     (Strm : access Ada.Streams.Root_Stream_Type'Class;
      Item : Wide_Wide_String);

   procedure Wide_Wide_String_Output_Blk_IO
     (Strm : access Ada.Streams.Root_Stream_Type'Class;
      Item : Wide_Wide_String);

   procedure Wide_Wide_String_Read
     (Strm : access Ada.Streams.Root_Stream_Type'Class;
      Item : out Wide_Wide_String);

   procedure Wide_Wide_String_Read_Blk_IO
     (Strm : access Ada.Streams.Root_Stream_Type'Class;
      Item : out Wide_Wide_String);

   procedure Wide_Wide_String_Write
     (Strm : access Ada.Streams.Root_Stream_Type'Class;
      Item : Wide_Wide_String);

   procedure Wide_Wide_String_Write_Blk_IO
     (Strm : access Ada.Streams.Root_Stream_Type'Class;
      Item : Wide_Wide_String);

end System.Strings.Stream_Ops;

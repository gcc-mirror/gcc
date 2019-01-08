------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--              S Y S T E M . S T R I N G S . S T R E A M _ O P S           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2009-2019, Free Software Foundation, Inc.         --
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

--  This package provides subprogram implementations of stream attributes for
--  the following types using a "block IO" approach in which the entire data
--  item is written in one operation, instead of writing individual characters.

--     Ada.Stream_Element_Array
--     Ada.String
--     Ada.Wide_String
--     Ada.Wide_Wide_String
--     System.Storage_Array

--  Note: this routine is in Ada.Strings because historically it handled only
--  the string types. It is not worth moving it at this stage.

--  The compiler will generate references to the subprograms in this package
--  when expanding stream attributes for the above mentioned types. Example:

--     String'Output (Some_Stream, Some_String);

--  will be expanded into:

--     String_Output (Some_Stream, Some_String);
--       or
--     String_Output_Blk_IO (Some_Stream, Some_String);

--  String_Output form is used if pragma Restrictions (No_String_Optimziations)
--  is active, which requires element by element operations. The BLK_IO form
--  is used if this restriction is not set, allowing block optimization.

--  Note that if System.Stream_Attributes.Block_IO_OK is False, then the BLK_IO
--  form is treated as equivalent to the normal case, so that the optimization
--  is inhibited anyway, regardless of the setting of the restriction. This
--  handles versions of System.Stream_Attributes (in particular the XDR version
--  found in s-stratt-xdr) which do not permit block io optimization.

pragma Compiler_Unit_Warning;

with Ada.Streams;

with System.Storage_Elements;

package System.Strings.Stream_Ops is

   -------------------------------------
   -- Storage_Array stream operations --
   -------------------------------------

   function Storage_Array_Input
     (Strm : access Ada.Streams.Root_Stream_Type'Class)
      return System.Storage_Elements.Storage_Array;

   function Storage_Array_Input_Blk_IO
     (Strm : access Ada.Streams.Root_Stream_Type'Class)
      return System.Storage_Elements.Storage_Array;

   procedure Storage_Array_Output
     (Strm : access Ada.Streams.Root_Stream_Type'Class;
      Item : System.Storage_Elements.Storage_Array);

   procedure Storage_Array_Output_Blk_IO
     (Strm : access Ada.Streams.Root_Stream_Type'Class;
      Item : System.Storage_Elements.Storage_Array);

   procedure Storage_Array_Read
     (Strm : access Ada.Streams.Root_Stream_Type'Class;
      Item : out System.Storage_Elements.Storage_Array);

   procedure Storage_Array_Read_Blk_IO
     (Strm : access Ada.Streams.Root_Stream_Type'Class;
      Item : out System.Storage_Elements.Storage_Array);

   procedure Storage_Array_Write
     (Strm : access Ada.Streams.Root_Stream_Type'Class;
      Item : System.Storage_Elements.Storage_Array);

   procedure Storage_Array_Write_Blk_IO
     (Strm : access Ada.Streams.Root_Stream_Type'Class;
      Item : System.Storage_Elements.Storage_Array);

   --------------------------------------------
   -- Stream_Element_Array stream operations --
   --------------------------------------------

   function Stream_Element_Array_Input
     (Strm : access Ada.Streams.Root_Stream_Type'Class)
      return Ada.Streams.Stream_Element_Array;

   function Stream_Element_Array_Input_Blk_IO
     (Strm : access Ada.Streams.Root_Stream_Type'Class)
      return Ada.Streams.Stream_Element_Array;

   procedure Stream_Element_Array_Output
     (Strm : access Ada.Streams.Root_Stream_Type'Class;
      Item : Ada.Streams.Stream_Element_Array);

   procedure Stream_Element_Array_Output_Blk_IO
     (Strm : access Ada.Streams.Root_Stream_Type'Class;
      Item : Ada.Streams.Stream_Element_Array);

   procedure Stream_Element_Array_Read
     (Strm : access Ada.Streams.Root_Stream_Type'Class;
      Item : out Ada.Streams.Stream_Element_Array);

   procedure Stream_Element_Array_Read_Blk_IO
     (Strm : access Ada.Streams.Root_Stream_Type'Class;
      Item : out Ada.Streams.Stream_Element_Array);

   procedure Stream_Element_Array_Write
     (Strm : access Ada.Streams.Root_Stream_Type'Class;
      Item : Ada.Streams.Stream_Element_Array);

   procedure Stream_Element_Array_Write_Blk_IO
     (Strm : access Ada.Streams.Root_Stream_Type'Class;
      Item : Ada.Streams.Stream_Element_Array);

   ------------------------------
   -- String stream operations --
   ------------------------------

   function String_Input
     (Strm : access Ada.Streams.Root_Stream_Type'Class)
      return String;

   function String_Input_Blk_IO
     (Strm : access Ada.Streams.Root_Stream_Type'Class)
      return String;

   function String_Input_Tag
     (Strm : access Ada.Streams.Root_Stream_Type'Class)
      return String;
   --  Same as String_Input_Blk_IO, except raises an exception for overly long
   --  Strings. See expansion of Attribute_Input in Exp_Attr for details.

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

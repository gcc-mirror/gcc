------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--              S Y S T E M . S T R I N G S . S T R E A M _ O P S           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2008-2010, Free Software Foundation, Inc.         --
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

pragma Compiler_Unit;

with Ada.Streams;              use Ada.Streams;
with Ada.Streams.Stream_IO;    use Ada.Streams.Stream_IO;
with Ada.Unchecked_Conversion;

with System.Stream_Attributes; use System;

package body System.Strings.Stream_Ops is

   --  The following type describes the low-level IO mechanism used in package
   --  Stream_Ops_Internal.

   type IO_Kind is (Byte_IO, Block_IO);

   --  The following package provides an IO framework for strings. Depending
   --  on the version of System.Stream_Attributes as well as the size of
   --  formal parameter Character_Type, the package will either utilize block
   --  IO or character-by-character IO.

   generic
      type Character_Type is private;
      type String_Type is array (Positive range <>) of Character_Type;

   package Stream_Ops_Internal is
      function Input
        (Strm : access Root_Stream_Type'Class;
         IO   : IO_Kind) return String_Type;

      procedure Output
        (Strm : access Root_Stream_Type'Class;
         Item : String_Type;
         IO   : IO_Kind);

      procedure Read
        (Strm : access Root_Stream_Type'Class;
         Item : out String_Type;
         IO   : IO_Kind);

      procedure Write
        (Strm : access Root_Stream_Type'Class;
         Item : String_Type;
         IO   : IO_Kind);
   end Stream_Ops_Internal;

   -------------------------
   -- Stream_Ops_Internal --
   -------------------------

   package body Stream_Ops_Internal is

      --  The following value represents the number of BITS allocated for the
      --  default block used in string IO. The sizes of all other types are
      --  calculated relative to this value.

      Default_Block_Size : constant := 512 * 8;

      --  Shorthand notation for stream element and character sizes

      C_Size  : constant Integer := Character_Type'Size;
      SE_Size : constant Integer := Stream_Element'Size;

      --  The following constants describe the number of stream elements or
      --  characters that can fit into a default block.

      C_In_Default_Block  : constant Integer := Default_Block_Size / C_Size;
      SE_In_Default_Block : constant Integer := Default_Block_Size / SE_Size;

      --  Buffer types

      subtype Default_Block is Stream_Element_Array
        (1 .. Stream_Element_Offset (SE_In_Default_Block));

      subtype String_Block is String_Type (1 .. C_In_Default_Block);

      --  Conversions to and from Default_Block

      function To_Default_Block is
        new Ada.Unchecked_Conversion (String_Block, Default_Block);

      function To_String_Block is
        new Ada.Unchecked_Conversion (Default_Block, String_Block);

      -----------
      -- Input --
      -----------

      function Input
        (Strm : access Root_Stream_Type'Class;
         IO   : IO_Kind) return String_Type
      is
      begin
         if Strm = null then
            raise Constraint_Error;
         end if;

         declare
            Low  : Positive;
            High : Positive;

         begin
            --  Read the bounds of the string

            Positive'Read (Strm, Low);
            Positive'Read (Strm, High);

            declare
               Item : String_Type (Low .. High);

            begin
               --  Read the character content of the string

               Read (Strm, Item, IO);

               return Item;
            end;
         end;
      end Input;

      ------------
      -- Output --
      ------------

      procedure Output
        (Strm : access Root_Stream_Type'Class;
         Item : String_Type;
         IO   : IO_Kind)
      is
      begin
         if Strm = null then
            raise Constraint_Error;
         end if;

         --  Write the bounds of the string

         Positive'Write (Strm, Item'First);
         Positive'Write (Strm, Item'Last);

         --  Write the character content of the string

         Write (Strm, Item, IO);
      end Output;

      ----------
      -- Read --
      ----------

      procedure Read
        (Strm : access Root_Stream_Type'Class;
         Item : out String_Type;
         IO   : IO_Kind)
      is
      begin
         if Strm = null then
            raise Constraint_Error;
         end if;

         --  Nothing to do if the desired string is empty

         if Item'Length = 0 then
            return;
         end if;

         --  Block IO

         if IO = Block_IO
           and then Stream_Attributes.Block_IO_OK
         then
            declare
               --  Determine the size in BITS of the block necessary to contain
               --  the whole string.

               Block_Size : constant Natural :=
                              (Item'Last - Item'First + 1) * C_Size;

               --  Item can be larger than what the default block can store,
               --  determine the number of whole reads necessary to read the
               --  string.

               Blocks : constant Natural := Block_Size / Default_Block_Size;

               --  The size of Item may not be a multiple of the default block
               --  size, determine the size of the remaining chunk in BITS.

               Rem_Size : constant Natural :=
                            Block_Size mod Default_Block_Size;

               --  String indexes

               Low  : Positive := Item'First;
               High : Positive := Low + C_In_Default_Block - 1;

               --  End of stream error detection

               Last : Stream_Element_Offset := 0;
               Sum  : Stream_Element_Offset := 0;

            begin
               --  Step 1: If the string is too large, read in individual
               --  chunks the size of the default block.

               if Blocks > 0 then
                  declare
                     Block : Default_Block;

                  begin
                     for Counter in 1 .. Blocks loop
                        Read (Strm.all, Block, Last);
                        Item (Low .. High) := To_String_Block (Block);

                        Low  := High + 1;
                        High := Low + C_In_Default_Block - 1;
                        Sum  := Sum + Last;
                        Last := 0;
                     end loop;
                  end;
               end if;

               --  Step 2: Read in any remaining elements

               if Rem_Size > 0 then
                  declare
                     subtype Rem_Block is Stream_Element_Array
                       (1 .. Stream_Element_Offset (Rem_Size / SE_Size));

                     subtype Rem_String_Block is
                       String_Type (1 .. Rem_Size / C_Size);

                     function To_Rem_String_Block is new
                       Ada.Unchecked_Conversion (Rem_Block, Rem_String_Block);

                     Block : Rem_Block;

                  begin
                     Read (Strm.all, Block, Last);
                     Item (Low .. Item'Last) := To_Rem_String_Block (Block);

                     Sum := Sum + Last;
                  end;
               end if;

               --  Step 3: Potential error detection. The sum of all the
               --  chunks is less than we initially wanted to read. In other
               --  words, the stream does not contain enough elements to fully
               --  populate Item.

               if (Integer (Sum) * SE_Size) / C_Size < Item'Length then
                  raise End_Error;
               end if;
            end;

         --  Byte IO

         else
            declare
               C : Character_Type;

            begin
               for Index in Item'First .. Item'Last loop
                  Character_Type'Read (Strm, C);
                  Item (Index) := C;
               end loop;
            end;
         end if;
      end Read;

      -----------
      -- Write --
      -----------

      procedure Write
        (Strm : access Root_Stream_Type'Class;
         Item : String_Type;
         IO   : IO_Kind)
      is
      begin
         if Strm = null then
            raise Constraint_Error;
         end if;

         --  Nothing to do if the input string is empty

         if Item'Length = 0 then
            return;
         end if;

         --  Block IO

         if IO = Block_IO
           and then Stream_Attributes.Block_IO_OK
         then
            declare
               --  Determine the size in BITS of the block necessary to contain
               --  the whole string.

               Block_Size : constant Natural := Item'Length * C_Size;

               --  Item can be larger than what the default block can store,
               --  determine the number of whole writes necessary to output the
               --  string.

               Blocks : constant Natural := Block_Size / Default_Block_Size;

               --  The size of Item may not be a multiple of the default block
               --  size, determine the size of the remaining chunk.

               Rem_Size : constant Natural :=
                            Block_Size mod Default_Block_Size;

               --  String indexes

               Low  : Positive := Item'First;
               High : Positive := Low + C_In_Default_Block - 1;

            begin
               --  Step 1: If the string is too large, write out individual
               --  chunks the size of the default block.

               for Counter in 1 .. Blocks loop
                  Write (Strm.all, To_Default_Block (Item (Low .. High)));

                  Low  := High + 1;
                  High := Low + C_In_Default_Block - 1;
               end loop;

               --  Step 2: Write out any remaining elements

               if Rem_Size > 0 then
                  declare
                     subtype Rem_Block is Stream_Element_Array
                       (1 .. Stream_Element_Offset (Rem_Size / SE_Size));

                     subtype Rem_String_Block is
                       String_Type (1 .. Rem_Size / C_Size);

                     function To_Rem_Block is new
                       Ada.Unchecked_Conversion (Rem_String_Block, Rem_Block);

                  begin
                     Write (Strm.all, To_Rem_Block (Item (Low .. Item'Last)));
                  end;
               end if;
            end;

         --  Byte IO

         else
            for Index in Item'First .. Item'Last loop
               Character_Type'Write (Strm, Item (Index));
            end loop;
         end if;
      end Write;
   end Stream_Ops_Internal;

   --  Specific instantiations for all Ada string types

   package String_Ops is
     new Stream_Ops_Internal
       (Character_Type => Character,
        String_Type    => String);

   package Wide_String_Ops is
     new Stream_Ops_Internal
       (Character_Type => Wide_Character,
        String_Type    => Wide_String);

   package Wide_Wide_String_Ops is
     new Stream_Ops_Internal
       (Character_Type => Wide_Wide_Character,
        String_Type    => Wide_Wide_String);

   ------------------
   -- String_Input --
   ------------------

   function String_Input
     (Strm : access Ada.Streams.Root_Stream_Type'Class) return String
   is
   begin
      return String_Ops.Input (Strm, Byte_IO);
   end String_Input;

   -------------------------
   -- String_Input_Blk_IO --
   -------------------------

   function String_Input_Blk_IO
     (Strm : access Ada.Streams.Root_Stream_Type'Class) return String
   is
   begin
      return String_Ops.Input (Strm, Block_IO);
   end String_Input_Blk_IO;

   -------------------
   -- String_Output --
   -------------------

   procedure String_Output
     (Strm : access Ada.Streams.Root_Stream_Type'Class;
      Item : String)
   is
   begin
      String_Ops.Output (Strm, Item, Byte_IO);
   end String_Output;

   --------------------------
   -- String_Output_Blk_IO --
   --------------------------

   procedure String_Output_Blk_IO
     (Strm : access Ada.Streams.Root_Stream_Type'Class;
      Item : String)
   is
   begin
      String_Ops.Output (Strm, Item, Block_IO);
   end String_Output_Blk_IO;

   -----------------
   -- String_Read --
   -----------------

   procedure String_Read
     (Strm : access Ada.Streams.Root_Stream_Type'Class;
      Item : out String)
   is
   begin
      String_Ops.Read (Strm, Item, Byte_IO);
   end String_Read;

   ------------------------
   -- String_Read_Blk_IO --
   ------------------------

   procedure String_Read_Blk_IO
     (Strm : access Ada.Streams.Root_Stream_Type'Class;
      Item : out String)
   is
   begin
      String_Ops.Read (Strm, Item, Block_IO);
   end String_Read_Blk_IO;

   ------------------
   -- String_Write --
   ------------------

   procedure String_Write
     (Strm : access Ada.Streams.Root_Stream_Type'Class;
      Item : String)
   is
   begin
      String_Ops.Write (Strm, Item, Byte_IO);
   end String_Write;

   -------------------------
   -- String_Write_Blk_IO --
   -------------------------

   procedure String_Write_Blk_IO
     (Strm : access Ada.Streams.Root_Stream_Type'Class;
      Item : String)
   is
   begin
      String_Ops.Write (Strm, Item, Block_IO);
   end String_Write_Blk_IO;

   -----------------------
   -- Wide_String_Input --
   -----------------------

   function Wide_String_Input
     (Strm : access Ada.Streams.Root_Stream_Type'Class) return Wide_String
   is
   begin
      return Wide_String_Ops.Input (Strm, Byte_IO);
   end Wide_String_Input;

   ------------------------------
   -- Wide_String_Input_Blk_IO --
   ------------------------------

   function Wide_String_Input_Blk_IO
     (Strm : access Ada.Streams.Root_Stream_Type'Class) return Wide_String
   is
   begin
      return Wide_String_Ops.Input (Strm, Block_IO);
   end Wide_String_Input_Blk_IO;

   ------------------------
   -- Wide_String_Output --
   ------------------------

   procedure Wide_String_Output
     (Strm : access Ada.Streams.Root_Stream_Type'Class;
      Item : Wide_String)
   is
   begin
      Wide_String_Ops.Output (Strm, Item, Byte_IO);
   end Wide_String_Output;

   -------------------------------
   -- Wide_String_Output_Blk_IO --
   -------------------------------

   procedure Wide_String_Output_Blk_IO
     (Strm : access Ada.Streams.Root_Stream_Type'Class;
      Item : Wide_String)
   is
   begin
      Wide_String_Ops.Output (Strm, Item, Block_IO);
   end Wide_String_Output_Blk_IO;

   ----------------------
   -- Wide_String_Read --
   ----------------------

   procedure Wide_String_Read
     (Strm : access Ada.Streams.Root_Stream_Type'Class;
      Item : out Wide_String)
   is
   begin
      Wide_String_Ops.Read (Strm, Item, Byte_IO);
   end Wide_String_Read;

   -----------------------------
   -- Wide_String_Read_Blk_IO --
   -----------------------------

   procedure Wide_String_Read_Blk_IO
     (Strm : access Ada.Streams.Root_Stream_Type'Class;
      Item : out Wide_String)
   is
   begin
      Wide_String_Ops.Read (Strm, Item, Block_IO);
   end Wide_String_Read_Blk_IO;

   -----------------------
   -- Wide_String_Write --
   -----------------------

   procedure Wide_String_Write
     (Strm : access Ada.Streams.Root_Stream_Type'Class;
      Item : Wide_String)
   is
   begin
      Wide_String_Ops.Write (Strm, Item, Byte_IO);
   end Wide_String_Write;

   ------------------------------
   -- Wide_String_Write_Blk_IO --
   ------------------------------

   procedure Wide_String_Write_Blk_IO
     (Strm : access Ada.Streams.Root_Stream_Type'Class;
      Item : Wide_String)
   is
   begin
      Wide_String_Ops.Write (Strm, Item, Block_IO);
   end Wide_String_Write_Blk_IO;

   ----------------------------
   -- Wide_Wide_String_Input --
   ----------------------------

   function Wide_Wide_String_Input
     (Strm : access Ada.Streams.Root_Stream_Type'Class) return Wide_Wide_String
   is
   begin
      return Wide_Wide_String_Ops.Input (Strm, Byte_IO);
   end Wide_Wide_String_Input;

   -----------------------------------
   -- Wide_Wide_String_Input_Blk_IO --
   -----------------------------------

   function Wide_Wide_String_Input_Blk_IO
     (Strm : access Ada.Streams.Root_Stream_Type'Class) return Wide_Wide_String
   is
   begin
      return Wide_Wide_String_Ops.Input (Strm, Block_IO);
   end Wide_Wide_String_Input_Blk_IO;

   -----------------------------
   -- Wide_Wide_String_Output --
   -----------------------------

   procedure Wide_Wide_String_Output
     (Strm : access Ada.Streams.Root_Stream_Type'Class;
      Item : Wide_Wide_String)
   is
   begin
      Wide_Wide_String_Ops.Output (Strm, Item, Byte_IO);
   end Wide_Wide_String_Output;

   ------------------------------------
   -- Wide_Wide_String_Output_Blk_IO --
   ------------------------------------

   procedure Wide_Wide_String_Output_Blk_IO
     (Strm : access Ada.Streams.Root_Stream_Type'Class;
      Item : Wide_Wide_String)
   is
   begin
      Wide_Wide_String_Ops.Output (Strm, Item, Block_IO);
   end Wide_Wide_String_Output_Blk_IO;

   ---------------------------
   -- Wide_Wide_String_Read --
   ---------------------------

   procedure Wide_Wide_String_Read
     (Strm : access Ada.Streams.Root_Stream_Type'Class;
      Item : out Wide_Wide_String)
   is
   begin
      Wide_Wide_String_Ops.Read (Strm, Item, Byte_IO);
   end Wide_Wide_String_Read;

   ----------------------------------
   -- Wide_Wide_String_Read_Blk_IO --
   ----------------------------------

   procedure Wide_Wide_String_Read_Blk_IO
     (Strm : access Ada.Streams.Root_Stream_Type'Class;
      Item : out Wide_Wide_String)
   is
   begin
      Wide_Wide_String_Ops.Read (Strm, Item, Block_IO);
   end Wide_Wide_String_Read_Blk_IO;

   ----------------------------
   -- Wide_Wide_String_Write --
   ----------------------------

   procedure Wide_Wide_String_Write
     (Strm : access Ada.Streams.Root_Stream_Type'Class;
      Item : Wide_Wide_String)
   is
   begin
      Wide_Wide_String_Ops.Write (Strm, Item, Byte_IO);
   end Wide_Wide_String_Write;

   -----------------------------------
   -- Wide_Wide_String_Write_Blk_IO --
   -----------------------------------

   procedure Wide_Wide_String_Write_Blk_IO
     (Strm : access Ada.Streams.Root_Stream_Type'Class;
      Item : Wide_Wide_String)
   is
   begin
      Wide_Wide_String_Ops.Write (Strm, Item, Block_IO);
   end Wide_Wide_String_Write_Blk_IO;

end System.Strings.Stream_Ops;

------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--              S Y S T E M . S T R I N G S . S T R E A M _ O P S           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2008-2019, Free Software Foundation, Inc.         --
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

pragma Compiler_Unit_Warning;

with Ada.Streams;              use Ada.Streams;
with Ada.Streams.Stream_IO;    use Ada.Streams.Stream_IO;
with Ada.Unchecked_Conversion;

with System;                   use System;
with System.Storage_Elements;  use System.Storage_Elements;
with System.Stream_Attributes;

package body System.Strings.Stream_Ops is

   --  The following type describes the low-level IO mechanism used in package
   --  Stream_Ops_Internal.

   type IO_Kind is (Byte_IO, Block_IO);

   --  The following package provides an IO framework for strings. Depending
   --  on the version of System.Stream_Attributes as well as the size of
   --  formal parameter Element_Type, the package will either utilize block
   --  IO or element-by-element IO.

   generic
      type Element_Type is private;
      type Index_Type is range <>;
      type Array_Type is array (Index_Type range <>) of Element_Type;

   package Stream_Ops_Internal is
      function Input
        (Strm       : access Root_Stream_Type'Class;
         IO         : IO_Kind;
         Max_Length : Long_Integer := Long_Integer'Last) return Array_Type;
      --  Raises an exception if you try to read a String that is longer than
      --  Max_Length. See expansion of Attribute_Input in Exp_Attr for details.

      procedure Output
        (Strm : access Root_Stream_Type'Class;
         Item : Array_Type;
         IO   : IO_Kind);

      procedure Read
        (Strm : access Root_Stream_Type'Class;
         Item : out Array_Type;
         IO   : IO_Kind);

      procedure Write
        (Strm : access Root_Stream_Type'Class;
         Item : Array_Type;
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

      --  Shorthand notation for stream element and element type sizes

      ET_Size : constant Integer := Element_Type'Size;
      SE_Size : constant Integer := Stream_Element'Size;

      --  The following constants describe the number of array elements or
      --  stream elements that can fit into a default block.

      AE_In_Default_Block : constant Index_Type :=
                              Index_Type (Default_Block_Size / ET_Size);
      --  Number of array elements in a default block

      SE_In_Default_Block : constant Integer := Default_Block_Size / SE_Size;
      --  Number of storage elements in a default block

      --  Buffer types

      subtype Default_Block is Stream_Element_Array
        (1 .. Stream_Element_Offset (SE_In_Default_Block));

      subtype Array_Block is
        Array_Type (Index_Type range 1 .. AE_In_Default_Block);

      --  Conversions to and from Default_Block

      function To_Default_Block is
        new Ada.Unchecked_Conversion (Array_Block, Default_Block);

      function To_Array_Block is
        new Ada.Unchecked_Conversion (Default_Block, Array_Block);

      -----------
      -- Input --
      -----------

      function Input
        (Strm       : access Root_Stream_Type'Class;
         IO         : IO_Kind;
         Max_Length : Long_Integer := Long_Integer'Last) return Array_Type
      is
         pragma Unsuppress (All_Checks);
         --  The above makes T'Class'Input robust in the case of bad data. The
         --  declaration of Item below could raise Storage_Error if the length
         --  is too big.

      begin
         if Strm = null then
            raise Constraint_Error;
         end if;

         declare
            Low, High : Index_Type'Base;
         begin
            --  Read the bounds of the string. Note that they could be out of
            --  range of Index_Type in the case of empty arrays.

            Index_Type'Read (Strm, Low);
            Index_Type'Read (Strm, High);

            if Long_Integer (High) - Long_Integer (Low) > Max_Length then
               raise Constraint_Error;
            end if;

            --  Read the character content of the string

            declare
               Item : Array_Type (Low .. High);
            begin
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
         Item : Array_Type;
         IO   : IO_Kind)
      is
      begin
         if Strm = null then
            raise Constraint_Error;
         end if;

         --  Write the bounds of the string

         Index_Type'Write (Strm, Item'First);
         Index_Type'Write (Strm, Item'Last);

         --  Write the character content of the string

         Write (Strm, Item, IO);
      end Output;

      ----------
      -- Read --
      ----------

      procedure Read
        (Strm : access Root_Stream_Type'Class;
         Item : out Array_Type;
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

         if IO = Block_IO and then Stream_Attributes.Block_IO_OK then
            declare
               --  Determine the size in BITS of the block necessary to contain
               --  the whole string.

               Block_Size : constant Natural :=
                              Integer (Item'Last - Item'First + 1) * ET_Size;

               --  Item can be larger than what the default block can store,
               --  determine the number of whole reads necessary to read the
               --  string.

               Blocks : constant Natural := Block_Size / Default_Block_Size;

               --  The size of Item may not be a multiple of the default block
               --  size, determine the size of the remaining chunk in BITS.

               Rem_Size : constant Natural :=
                            Block_Size mod Default_Block_Size;

               --  String indexes

               Low  : Index_Type := Item'First;
               High : Index_Type := Low + AE_In_Default_Block - 1;

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
                        Item (Low .. High) := To_Array_Block (Block);

                        Low  := High + 1;
                        High := Low + AE_In_Default_Block - 1;
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

                     subtype Rem_Array_Block is
                       Array_Type (Index_Type range
                                    1 .. Index_Type (Rem_Size / ET_Size));

                     function To_Rem_Array_Block is new
                       Ada.Unchecked_Conversion (Rem_Block, Rem_Array_Block);

                     Block : Rem_Block;

                  begin
                     Read (Strm.all, Block, Last);
                     Item (Low .. Item'Last) := To_Rem_Array_Block (Block);

                     Sum := Sum + Last;
                  end;
               end if;

               --  Step 3: Potential error detection. The sum of all the
               --  chunks is less than we initially wanted to read. In other
               --  words, the stream does not contain enough elements to fully
               --  populate Item.

               if (Integer (Sum) * SE_Size) / ET_Size < Item'Length then
                  raise End_Error;
               end if;
            end;

         --  Byte IO

         else
            declare
               E : Element_Type;
            begin
               for Index in Item'First .. Item'Last loop
                  Element_Type'Read (Strm, E);
                  Item (Index) := E;
               end loop;
            end;
         end if;
      end Read;

      -----------
      -- Write --
      -----------

      procedure Write
        (Strm : access Root_Stream_Type'Class;
         Item : Array_Type;
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

         if IO = Block_IO and then Stream_Attributes.Block_IO_OK then
            declare
               --  Determine the size in BITS of the block necessary to contain
               --  the whole string.

               Block_Size : constant Natural := Item'Length * ET_Size;

               --  Item can be larger than what the default block can store,
               --  determine the number of whole writes necessary to output the
               --  string.

               Blocks : constant Natural := Block_Size / Default_Block_Size;

               --  The size of Item may not be a multiple of the default block
               --  size, determine the size of the remaining chunk.

               Rem_Size : constant Natural :=
                            Block_Size mod Default_Block_Size;

               --  String indexes

               Low  : Index_Type := Item'First;
               High : Index_Type := Low + AE_In_Default_Block - 1;

            begin
               --  Step 1: If the string is too large, write out individual
               --  chunks the size of the default block.

               for Counter in 1 .. Blocks loop
                  Write (Strm.all, To_Default_Block (Item (Low .. High)));
                  Low  := High + 1;
                  High := Low + AE_In_Default_Block - 1;
               end loop;

               --  Step 2: Write out any remaining elements

               if Rem_Size > 0 then
                  declare
                     subtype Rem_Block is Stream_Element_Array
                       (1 .. Stream_Element_Offset (Rem_Size / SE_Size));

                     subtype Rem_Array_Block is
                       Array_Type (Index_Type range
                                     1 .. Index_Type (Rem_Size / ET_Size));

                     function To_Rem_Block is new
                       Ada.Unchecked_Conversion (Rem_Array_Block, Rem_Block);

                  begin
                     Write (Strm.all, To_Rem_Block (Item (Low .. Item'Last)));
                  end;
               end if;
            end;

         --  Byte IO

         else
            for Index in Item'First .. Item'Last loop
               Element_Type'Write (Strm, Item (Index));
            end loop;
         end if;
      end Write;
   end Stream_Ops_Internal;

   --  Specific instantiations for all Ada array types handled

   package Storage_Array_Ops is
     new Stream_Ops_Internal
       (Element_Type => Storage_Element,
        Index_Type   => Storage_Offset,
        Array_Type   => Storage_Array);

   package Stream_Element_Array_Ops is
     new Stream_Ops_Internal
       (Element_Type => Stream_Element,
        Index_Type   => Stream_Element_Offset,
        Array_Type   => Stream_Element_Array);

   package String_Ops is
     new Stream_Ops_Internal
       (Element_Type => Character,
        Index_Type   => Positive,
        Array_Type   => String);

   package Wide_String_Ops is
     new Stream_Ops_Internal
       (Element_Type => Wide_Character,
        Index_Type   => Positive,
        Array_Type   => Wide_String);

   package Wide_Wide_String_Ops is
     new Stream_Ops_Internal
       (Element_Type => Wide_Wide_Character,
        Index_Type   => Positive,
        Array_Type   => Wide_Wide_String);

   -------------------------
   -- Storage_Array_Input --
   -------------------------

   function Storage_Array_Input
     (Strm : access Ada.Streams.Root_Stream_Type'Class) return Storage_Array
   is
   begin
      return Storage_Array_Ops.Input (Strm, Byte_IO);
   end Storage_Array_Input;

   --------------------------------
   -- Storage_Array_Input_Blk_IO --
   --------------------------------

   function Storage_Array_Input_Blk_IO
     (Strm : access Ada.Streams.Root_Stream_Type'Class) return Storage_Array
   is
   begin
      return Storage_Array_Ops.Input (Strm, Block_IO);
   end Storage_Array_Input_Blk_IO;

   --------------------------
   -- Storage_Array_Output --
   --------------------------

   procedure Storage_Array_Output
     (Strm : access Ada.Streams.Root_Stream_Type'Class;
      Item : Storage_Array)
   is
   begin
      Storage_Array_Ops.Output (Strm, Item, Byte_IO);
   end Storage_Array_Output;

   ---------------------------------
   -- Storage_Array_Output_Blk_IO --
   ---------------------------------

   procedure Storage_Array_Output_Blk_IO
     (Strm : access Ada.Streams.Root_Stream_Type'Class;
      Item : Storage_Array)
   is
   begin
      Storage_Array_Ops.Output (Strm, Item, Block_IO);
   end Storage_Array_Output_Blk_IO;

   ------------------------
   -- Storage_Array_Read --
   ------------------------

   procedure Storage_Array_Read
     (Strm : access Ada.Streams.Root_Stream_Type'Class;
      Item : out Storage_Array)
   is
   begin
      Storage_Array_Ops.Read (Strm, Item, Byte_IO);
   end Storage_Array_Read;

   -------------------------------
   -- Storage_Array_Read_Blk_IO --
   -------------------------------

   procedure Storage_Array_Read_Blk_IO
     (Strm : access Ada.Streams.Root_Stream_Type'Class;
      Item : out Storage_Array)
   is
   begin
      Storage_Array_Ops.Read (Strm, Item, Block_IO);
   end Storage_Array_Read_Blk_IO;

   -------------------------
   -- Storage_Array_Write --
   -------------------------

   procedure Storage_Array_Write
     (Strm : access Ada.Streams.Root_Stream_Type'Class;
      Item : Storage_Array)
   is
   begin
      Storage_Array_Ops.Write (Strm, Item, Byte_IO);
   end Storage_Array_Write;

   --------------------------------
   -- Storage_Array_Write_Blk_IO --
   --------------------------------

   procedure Storage_Array_Write_Blk_IO
     (Strm : access Ada.Streams.Root_Stream_Type'Class;
      Item : Storage_Array)
   is
   begin
      Storage_Array_Ops.Write (Strm, Item, Block_IO);
   end Storage_Array_Write_Blk_IO;

   --------------------------------
   -- Stream_Element_Array_Input --
   --------------------------------

   function Stream_Element_Array_Input
     (Strm : access Ada.Streams.Root_Stream_Type'Class)
      return Stream_Element_Array
   is
   begin
      return Stream_Element_Array_Ops.Input (Strm, Byte_IO);
   end Stream_Element_Array_Input;

   ---------------------------------------
   -- Stream_Element_Array_Input_Blk_IO --
   ---------------------------------------

   function Stream_Element_Array_Input_Blk_IO
     (Strm : access Ada.Streams.Root_Stream_Type'Class)
      return Stream_Element_Array
   is
   begin
      return Stream_Element_Array_Ops.Input (Strm, Block_IO);
   end Stream_Element_Array_Input_Blk_IO;

   ---------------------------------
   -- Stream_Element_Array_Output --
   ---------------------------------

   procedure Stream_Element_Array_Output
     (Strm : access Ada.Streams.Root_Stream_Type'Class;
      Item : Stream_Element_Array)
   is
   begin
      Stream_Element_Array_Ops.Output (Strm, Item, Byte_IO);
   end Stream_Element_Array_Output;

   ----------------------------------------
   -- Stream_Element_Array_Output_Blk_IO --
   ----------------------------------------

   procedure Stream_Element_Array_Output_Blk_IO
     (Strm : access Ada.Streams.Root_Stream_Type'Class;
      Item : Stream_Element_Array)
   is
   begin
      Stream_Element_Array_Ops.Output (Strm, Item, Block_IO);
   end Stream_Element_Array_Output_Blk_IO;

   -------------------------------
   -- Stream_Element_Array_Read --
   -------------------------------

   procedure Stream_Element_Array_Read
     (Strm : access Ada.Streams.Root_Stream_Type'Class;
      Item : out Stream_Element_Array)
   is
   begin
      Stream_Element_Array_Ops.Read (Strm, Item, Byte_IO);
   end Stream_Element_Array_Read;

   --------------------------------------
   -- Stream_Element_Array_Read_Blk_IO --
   --------------------------------------

   procedure Stream_Element_Array_Read_Blk_IO
     (Strm : access Ada.Streams.Root_Stream_Type'Class;
      Item : out Stream_Element_Array)
   is
   begin
      Stream_Element_Array_Ops.Read (Strm, Item, Block_IO);
   end Stream_Element_Array_Read_Blk_IO;

   --------------------------------
   -- Stream_Element_Array_Write --
   --------------------------------

   procedure Stream_Element_Array_Write
     (Strm : access Ada.Streams.Root_Stream_Type'Class;
      Item : Stream_Element_Array)
   is
   begin
      Stream_Element_Array_Ops.Write (Strm, Item, Byte_IO);
   end Stream_Element_Array_Write;

   ---------------------------------------
   -- Stream_Element_Array_Write_Blk_IO --
   ---------------------------------------

   procedure Stream_Element_Array_Write_Blk_IO
     (Strm : access Ada.Streams.Root_Stream_Type'Class;
      Item : Stream_Element_Array)
   is
   begin
      Stream_Element_Array_Ops.Write (Strm, Item, Block_IO);
   end Stream_Element_Array_Write_Blk_IO;

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

   -------------------------
   -- String_Input_Tag --
   -------------------------

   function String_Input_Tag
     (Strm : access Ada.Streams.Root_Stream_Type'Class) return String
   is
   begin
      return String_Ops.Input (Strm, Block_IO, Max_Length => 10_000);
   end String_Input_Tag;

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

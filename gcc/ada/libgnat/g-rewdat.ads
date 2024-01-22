------------------------------------------------------------------------------
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     G N A T . R E W R I T E _ D A T A                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--           Copyright (C) 2014-2024, Free Software Foundation, Inc.        --
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

--  This package can be used to rewrite data on the fly. All occurrences of a
--  string (named pattern) will be replaced by another string.

--  It is not necessary to load all data in memory and so this package can be
--  used for large data chunks like disk files for example. The pattern is
--  a standard string and not a regular expression.

--  There is no dynamic allocation in the implementation.

--  For example, to replace all occurrences of "Gnat" with "GNAT":

--    Rewriter : Buffer := Create (Pattern => "Gnat", Value => "GNAT");

--  The output procedure that will receive the rewritten data:

--    procedure Do (Data : Stream_Element_Array) is
--    begin
--       <implementation to handle Data>
--    end Do;

--  Then:

--    Write (Rewriter, "Let's talk about Gnat compiler", Do'Access);
--    Write (Rewriter, "Gnat is an Ada compiler", Do'Access);
--    Flush (Rewriter, Do'Access);

--  Another possible usage is to specify a method to get the input data:

--    procedure Get
--      (Buffer : out Stream_Element_Array;
--       Last   : out Stream_Element_Offset)
--    is
--    begin
--       <get some data from a file, a socket, etc...>
--       Last := ...
--       Buffer := ...
--    end Get;

--  Then we can rewrite the whole file with:

--    Rewrite (Rewriter, Input => Get'Access, Output => Do'Access);

with Ada.Streams; use Ada.Streams;

package GNAT.Rewrite_Data is

   type Buffer (<>) is limited private;
   type Buffer_Ref is access all Buffer;

   function Create
     (Pattern, Value : String;
      Size           : Stream_Element_Offset := 1_024) return Buffer;
   --  Create a rewrite buffer. Pattern is the string to be rewritten as Value.
   --  Size represents the size of the internal buffer used to store the data
   --  ready to be output. A larger buffer may improve the performance, as the
   --  Output routine (see Write, Rewrite below) will be called only when this
   --  buffer is full. Note that Size cannot be lower than Pattern'Length, and
   --  if this is the case, then Size value is set to Pattern'Length.

   function Size (B : Buffer) return Natural;
   --  Returns the current size of the buffer (count of Stream_Array_Element)

   procedure Flush
     (B      : in out Buffer;
      Output : not null access procedure (Data : Stream_Element_Array));
   --  Call Output for all remaining data in the buffer. The buffer is
   --  reset and ready for another use after this call.

   procedure Reset (B : in out Buffer);
   pragma Inline (Reset);
   --  Clear all data in buffer, B is ready for another use. Note that this is
   --  not needed after a Flush. Note: all data remaining in Buffer is lost.

   procedure Write
     (B      : in out Buffer;
      Data   : Stream_Element_Array;
      Output : not null access procedure (Data : Stream_Element_Array));
   --  Write Data into the buffer, call Output for any prepared data. Flush
   --  must be called when the last piece of Data as been sent in the Buffer.

   procedure Rewrite
     (B      : in out Buffer;
      Input  : not null access procedure
                          (Buffer : out Stream_Element_Array;
                           Last   : out Stream_Element_Offset);
      Output : not null access procedure (Data : Stream_Element_Array));
   --  Read data from Input, rewrite it, and then call Output. When there is
   --  no more data to be read from Input, Last must be set to 0. Before
   --  leaving this routine, call Flush above to send all remaining data to
   --  Output.

   procedure Link (From : in out Buffer; To : Buffer_Ref);
   --  Link two rewrite buffers. That is, all data sent to From buffer will be
   --  rewritten and then passed to the To rewrite buffer.

private

   type Buffer
     (Size, Size_Pattern, Size_Value : Stream_Element_Offset) is
   limited record
      Pos_C : Stream_Element_Offset; -- last valid element in Current
      Pos_B : Stream_Element_Offset; -- last valid element in Buffer

      Next  : Buffer_Ref;
      --  A link to another rewriter if any

      Buffer : Stream_Element_Array (1 .. Size);
      --  Fully prepared/rewritten data waiting to be output

      Current : Stream_Element_Array (1 .. Size_Pattern);
      --  Current data checked, this buffer contains every piece of data
      --  starting with the pattern. It means that at any point:
      --  Current (1 .. Pos_C) = Pattern (1 .. Pos_C).

      Pattern : Stream_Element_Array (1 .. Size_Pattern);
      --  The pattern to look for

      Value : Stream_Element_Array (1 .. Size_Value);
      --  The value the pattern is replaced by
   end record;

end GNAT.Rewrite_Data;

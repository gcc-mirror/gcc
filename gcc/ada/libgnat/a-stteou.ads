------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                          ADA.STRINGS.TEXT_OUTPUT                         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2020, Free Software Foundation, Inc.            --
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

pragma Ada_2020;

with Ada.Strings.UTF_Encoding;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
package Ada.Strings.Text_Output with Preelaborate is

   --  This package provides a "Sink" abstraction, to which characters of type
   --  Character, Wide_Character, and Wide_Wide_Character can be sent. This
   --  type is used by the Put_Image attribute. In particular, T'Put_Image has
   --  the following parameter types:
   --
   --     procedure T'Put_Image (S : in out Sink'Class; V : T);
   --
   --  The default generated code for Put_Image of a composite type will
   --  typically call Put_Image on the components.
   --
   --  This is not a fully general abstraction that can be arbitrarily
   --  extended. It is designed with particular extensions in mind, and these
   --  extensions are declared in child packages of this package, because they
   --  depend on implementation details in the private part of this
   --  package. The primary extensions of Sink are:
   --
   --     Buffer. The characters sent to a Buffer are stored in memory, and can
   --     be retrieved via Get functions. This is intended for the
   --     implementation of the 'Image attribute. The compiler will generate a
   --     T'Image function that declares a local Buffer, sends characters to
   --     it, and then returns a call to Get, Destroying the Buffer on return.
   --
   --       function T'Image (V : T) return String is
   --          Buf : Buffer := New_Buffer (...);
   --       begin
   --          T'Put_Image (Buf, V);
   --          return Result : constant String := Get (Buf) do
   --             Destroy (Buf);
   --          end return;
   --       end T'Image;
   --       ????Perhaps Buffer should be controlled; if you don't like
   --       controlled types, call Put_Image directly.
   --
   --     File. The characters are sent to a file, possibly opened by file
   --     name, or possibly standard output or standard error. 'Put_Image
   --     can be called directly on a File, thus avoiding any heap allocation.

   type Sink (<>) is abstract tagged limited private;
   type Sink_Access is access all Sink'Class with Storage_Size => 0;
   --  Sink is a character sink; you can send characters to a Sink.
   --  UTF-8 encoding is used.

   procedure Full_Method (S : in out Sink) is abstract;
   procedure Flush_Method (S : in out Sink) is abstract;
   --  There is an internal buffer to store the characters. Full_Method is
   --  called when the buffer is full, and Flush_Method may be called to flush
   --  the buffer. For Buffer, Full_Method allocates more space for more
   --  characters, and Flush_Method does nothing. For File, Full_Method and
   --  Flush_Method do the same thing: write the characters to the file, and
   --  empty the internal buffer.
   --
   --  These are the only dispatching subprograms on Sink. This is for
   --  efficiency; we don't dispatch on every write to the Sink, but only when
   --  the internal buffer is full (or upon client request).
   --
   --  Full_Method and Flush_Method must make the current chunk empty.
   --
   --  Additional operations operating on Sink'Class are declared in the Utils
   --  child, including Full and Flush, which call the above.

   function To_Wide (C : Character) return Wide_Character is
     (Wide_Character'Val (Character'Pos (C)));
   function To_Wide_Wide (C : Character) return Wide_Wide_Character is
     (Wide_Wide_Character'Val (Character'Pos (C)));
   function To_Wide_Wide (C : Wide_Character) return Wide_Wide_Character is
     (Wide_Wide_Character'Val (Wide_Character'Pos (C)));
   --  Conversions [Wide_]Character --> [Wide_]Wide_Character.
   --  These cannot fail.

   function From_Wide (C : Wide_Character) return Character is
     (Character'Val (Wide_Character'Pos (C)));
   function From_Wide_Wide (C : Wide_Wide_Character) return Character is
     (Character'Val (Wide_Wide_Character'Pos (C)));
   function From_Wide_Wide (C : Wide_Wide_Character) return Wide_Character is
     (Wide_Character'Val (Wide_Wide_Character'Pos (C)));
   --  Conversions [Wide_]Wide_Character --> [Wide_]Character.
   --  These fail if the character is out of range.

   function NL return Character is (ASCII.LF) with Inline;
   function Wide_NL return Wide_Character is (To_Wide (Character'(NL)))
     with Inline;
   function Wide_Wide_NL return Wide_Wide_Character is
     (To_Wide_Wide (Character'(NL))) with Inline;
   --  Character representing new line. There is no support for CR/LF line
   --  endings.

   --  We have two subtypes of String that are encoded in UTF-8. UTF_8 cannot
   --  contain newline characters; UTF_8_Lines can. Sending UTF_8 data to a
   --  Sink is more efficient, because end-of-line processing is not needed.
   --  Both of these are more efficient than [[Wide_]Wide_]String, because no
   --  encoding is needed.

   subtype UTF_8_Lines is UTF_Encoding.UTF_8_String with
     Predicate =>
       UTF_Encoding.Wide_Wide_Strings.Encode
         (UTF_Encoding.Wide_Wide_Strings.Decode (UTF_8_Lines)) = UTF_8_Lines;

   subtype UTF_8 is UTF_8_Lines with
     Predicate => (for all UTF_8_Char of UTF_8 => UTF_8_Char /= NL);

   Default_Indent_Amount : constant Natural := 4;

   Default_Chunk_Length : constant Positive := 500;
   --  Experiment shows this value to be reasonably efficient; decreasing it
   --  slows things down, but increasing it doesn't gain much.

private
   type String_Access is access all String;

   --  For Buffer, the "internal buffer" mentioned above is implemented as a
   --  linked list of chunks. When the current chunk is full, we allocate a new
   --  one. For File, there is only one chunk. When it is full, we send the
   --  data to the file, and empty it.

   type Chunk;
   type Chunk_Access is access all Chunk;
   type Chunk (Length : Positive) is limited record
      Next : Chunk_Access := null;
      Chars : UTF_8_Lines (1 .. Length);
   end record;

   type Sink (Chunk_Length : Positive) is abstract tagged limited record
      Indent_Amount : Natural;
      Column : Positive := 1;
      Indentation : Natural := 0;

      All_7_Bits : Boolean := True;
      --  For optimization of Text_Output.Buffers.Get (cf).
      --  True if all characters seen so far fit in 7 bits.
      --  7-bit characters are represented the same in Character
      --  and in UTF-8, so they don't need translation.

      All_8_Bits : Boolean := True;
      --  True if all characters seen so far fit in 8 bits.
      --  This is needed in Text_Output.Buffers.Get to distinguish
      --  the case where all characters are Latin-1 (so it should
      --  decode) from the case where some characters are bigger than
      --  8 bits (so the result is implementation defined).

      Cur_Chunk : Chunk_Access;
      --  Points to the chunk we are currently sending characters to.
      --  We want to say:
      --     Cur_Chunk : Chunk_Access := Initial_Chunk'Access;
      --  but that's illegal, so we have some horsing around to do.

      Last : Natural := 0;
      --  Last-used character in Cur_Chunk.all.

      Initial_Chunk : aliased Chunk (Length => Chunk_Length);
      --  For Buffer, this is the first chunk. Subsequent chunks are allocated
      --  on the heap. For File, this is the only chunk, and there is no heap
      --  allocation.
   end record;

end Ada.Strings.Text_Output;

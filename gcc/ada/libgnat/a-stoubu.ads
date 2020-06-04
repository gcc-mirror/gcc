------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                      ADA.STRINGS.TEXT_OUTPUT.BUFFERS                     --
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

package Ada.Strings.Text_Output.Buffers is

   type Buffer (<>) is new Sink with private;

   function New_Buffer
     (Indent_Amount : Natural := Default_Indent_Amount;
      Chunk_Length : Positive := Default_Chunk_Length) return Buffer;

   procedure Destroy (S : in out Buffer);
   --  Reclaim any heap-allocated data, and render the Buffer unusable.
   --  It would make sense to do this via finalization, but we wish to
   --  avoid controlled types in the generated code for 'Image.

   function Get_UTF_8 (S : Buffer'Class) return UTF_8_Lines;
   --  Get the characters in S, encoded as UTF-8.

   function UTF_8_Length (S : Buffer'Class) return Natural;
   procedure Get_UTF_8
     (S : Buffer'Class; Result : out UTF_8_Lines) with
       Pre => Result'First = 1 and Result'Last = UTF_8_Length (S);
   --  This is a procedure version of the Get_UTF_8 function, for
   --  efficiency. The Result String must be the exact right length.

   function Get (S : Buffer'Class) return String;
   function Wide_Get (S : Buffer'Class) return Wide_String;
   function Wide_Wide_Get (S : Buffer'Class) return Wide_Wide_String;
   --  Get the characters in S, decoded as [[Wide_]Wide_]String.
   --  There is no need for procedure versions of these, because
   --  they are intended primarily to implement the [[Wide_]Wide_]Image
   --  attribute, which is already a function.

private
   type Chunk_Count is new Natural;
   type Buffer is new Sink with record
      Num_Extra_Chunks : Natural := 0;
      --  Number of chunks in the linked list, not including Initial_Chunk.
   end record;

   overriding procedure Full_Method (S : in out Buffer);
   overriding procedure Flush_Method (S : in out Buffer) is null;

end Ada.Strings.Text_Output.Buffers;

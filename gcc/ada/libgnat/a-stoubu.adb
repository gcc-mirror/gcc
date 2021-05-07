------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                      ADA.STRINGS.TEXT_OUTPUT.BUFFERS                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2020-2021, Free Software Foundation, Inc.       --
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

with Unchecked_Deallocation;
with Ada.Strings.UTF_Encoding.Strings;
with Ada.Strings.UTF_Encoding.Wide_Strings;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
package body Ada.Strings.Text_Output.Buffers is

   type Chunk_Access is access all Chunk;

   function New_Buffer
     (Indent_Amount : Natural := Default_Indent_Amount;
      Chunk_Length : Positive := Default_Chunk_Length) return Buffer
   is
   begin
      return Result : Buffer (Chunk_Length) do
         Result.Indent_Amount := Indent_Amount;
         Result.Cur_Chunk := Result.Initial_Chunk'Unchecked_Access;
      end return;
   end New_Buffer;

   --  We need type conversions of Chunk_Access values in the following two
   --  procedures, because the one in Text_Output has Storage_Size => 0,
   --  because Text_Output is Pure. We do not run afoul of 13.11.2(16/3),
   --  which requires the allocation and deallocation to have the same pool,
   --  because the allocation in Full_Method and the deallocation in Destroy
   --  use the same access type, and therefore the same pool.

   procedure Destroy (S : in out Buffer) is
      procedure Free is new Unchecked_Deallocation (Chunk, Chunk_Access);
      Cur : Chunk_Access := Chunk_Access (S.Initial_Chunk.Next);
   begin
      while Cur /= null loop
         declare
            Temp : constant Chunk_Access := Chunk_Access (Cur.Next);
         begin
            Free (Cur);
            Cur := Temp;
         end;
      end loop;

      S.Cur_Chunk := null;
   end Destroy;

   overriding procedure Full_Method (S : in out Buffer) is
   begin
      pragma Assert (S.Cur_Chunk.Next = null);
      pragma Assert (S.Last = S.Cur_Chunk.Chars'Length);
      S.Cur_Chunk.Next :=
        Text_Output.Chunk_Access (Chunk_Access'(new Chunk (S.Chunk_Length)));
      S.Cur_Chunk := S.Cur_Chunk.Next;
      S.Num_Extra_Chunks := S.Num_Extra_Chunks + 1;
      S.Last := 0;
   end Full_Method;

   function UTF_8_Length (S : Buffer'Class) return Natural is
   begin
      return S.Num_Extra_Chunks * S.Chunk_Length + S.Last;
   end UTF_8_Length;

   procedure Get_UTF_8
     (S : Buffer'Class; Result : out UTF_8_Lines)
   is
      Cur : access constant Chunk := S.Initial_Chunk'Access;
      First : Positive := 1;
   begin
      loop
         if Cur.Next = null then
            pragma Assert (Result'Last = First + S.Last - 1);
            Result (First .. Result'Last) := Cur.Chars (1 .. S.Last);
            exit;
         end if;

         pragma Assert (S.Chunk_Length = Cur.Chars'Length);
         Result (First .. First + S.Chunk_Length - 1) := Cur.Chars;
         First := First + S.Chunk_Length;
         Cur := Cur.Next;
      end loop;
   end Get_UTF_8;

   function Get_UTF_8 (S : Buffer'Class) return UTF_8_Lines is
   begin
      return Result : String (1 .. UTF_8_Length (S)) do
         Get_UTF_8 (S, Result);
      end return;
   end Get_UTF_8;

   function Get (S : Buffer'Class) return String is
   begin
      --  If all characters are 7 bits, we don't need to decode;
      --  this is an optimization.

      --  Otherwise, if all are 8 bits, we need to decode to get Latin-1.
      --  Otherwise, the result is implementation defined, so we return a
      --  String encoded as UTF-8. (Note that the AI says "if any character
      --  in the sequence is not defined in Character, the result is
      --  implementation-defined", so we are not obliged to decode ANY
      --  Latin-1 characters if ANY character is bigger than 8 bits.

      if S.All_7_Bits then
         return Get_UTF_8 (S);
      elsif S.All_8_Bits then
         return UTF_Encoding.Strings.Decode (Get_UTF_8 (S));
      else
         return Get_UTF_8 (S);
      end if;
   end Get;

   function Wide_Get (S : Buffer'Class) return Wide_String is
   begin
      return UTF_Encoding.Wide_Strings.Decode (Get_UTF_8 (S));
   end Wide_Get;

   function Wide_Wide_Get (S : Buffer'Class) return Wide_Wide_String is
   begin
      return UTF_Encoding.Wide_Wide_Strings.Decode (Get_UTF_8 (S));
   end Wide_Wide_Get;

end Ada.Strings.Text_Output.Buffers;

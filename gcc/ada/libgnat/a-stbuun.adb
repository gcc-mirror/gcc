------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                    ADA.STRINGS.TEXT_BUFFERS.UNBOUNDED                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2020-2025, Free Software Foundation, Inc.       --
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

with Ada.Characters.Handling;
with Ada.Unchecked_Deallocation;
with Ada.Strings.UTF_Encoding.Conversions;
with Ada.Strings.UTF_Encoding.Strings;
with Ada.Strings.UTF_Encoding.Wide_Strings;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
package body Ada.Strings.Text_Buffers.Unbounded is

   function Get (Buffer : in out Buffer_Type) return String is
   --  If all characters are 7 bits, we don't need to decode;
   --  this is an optimization.
   --  Otherwise, if all are 8 bits, we need to decode to get Latin-1.
   --  Otherwise, the result is implementation defined, so we return a
   --  String encoded as UTF-8. Note that the RM says "if any character
   --  in the sequence is not defined in Character, the result is
   --  implementation-defined", so we are not obliged to decode ANY
   --  Latin-1 characters if ANY character is bigger than 8 bits.
   begin
      if Buffer.All_8_Bits and not Buffer.All_7_Bits then
         return UTF_Encoding.Strings.Decode (Get_UTF_8 (Buffer));
      else
         return Get_UTF_8 (Buffer);
      end if;
   end Get;

   function Wide_Get (Buffer : in out Buffer_Type) return Wide_String is
   begin
      return UTF_Encoding.Wide_Strings.Decode (Get_UTF_8 (Buffer));
   end Wide_Get;

   function Wide_Wide_Get (Buffer : in out Buffer_Type) return Wide_Wide_String
   is
   begin
      return UTF_Encoding.Wide_Wide_Strings.Decode (Get_UTF_8 (Buffer));
   end Wide_Wide_Get;

   function Get_UTF_8
     (Buffer : in out Buffer_Type) return UTF_Encoding.UTF_8_String
   is
   begin
      return Result : UTF_Encoding.UTF_8_String (1 .. Buffer.UTF_8_Length) do
         declare
            Target_First : Positive := 1;
            Ptr : Chunk_Access := Buffer.List.First_Chunk'Unchecked_Access;
            Target_Last  : Positive;
         begin
            while Ptr /= null loop
               Target_Last := Target_First + Ptr.Chars'Length - 1;
               if Target_Last <= Result'Last then
                  --  all of chunk is assigned to Result
                  Result (Target_First .. Target_Last) := Ptr.Chars;
                  Target_First := Target_First + Ptr.Chars'Length;
               else
                  --  only part of (last) chunk is assigned to Result
                  declare
                     Final_Target : UTF_Encoding.UTF_8_String renames
                       Result (Target_First .. Result'Last);
                  begin
                     Final_Target := Ptr.Chars (1 .. Final_Target'Length);
                  end;
                  pragma Assert (Ptr.Next = null);
                  Target_First := Integer'Last;
               end if;

               Ptr := Ptr.Next;
            end loop;
         end;

         --  Reset buffer to default initial value.
         declare
            Defaulted : Buffer_Type;

            --  If this aggregate becomes illegal due to new field, don't
            --  forget to add corresponding assignment statement below.
            Dummy : array (1 .. 0) of Buffer_Type :=
              [others =>
                 (Indentation               => <>,
                  Indent_Pending            => <>,
                  UTF_8_Length              => <>,
                  UTF_8_Column              => <>,
                  All_7_Bits                => <>,
                  All_8_Bits                => <>,
                  Trim_Leading_White_Spaces => <>,
                  List                      => <>,
                  Last_Used                 => <>)];
         begin
            Buffer.Indentation    := Defaulted.Indentation;
            Buffer.Indent_Pending := Defaulted.Indent_Pending;
            Buffer.UTF_8_Length   := Defaulted.UTF_8_Length;
            Buffer.UTF_8_Column   := Defaulted.UTF_8_Column;
            Buffer.All_7_Bits     := Defaulted.All_7_Bits;
            Buffer.All_8_Bits     := Defaulted.All_8_Bits;
            Buffer.Last_Used      := Defaulted.Last_Used;
            Finalize (Buffer.List); -- free any allocated chunks
         end;
      end return;
   end Get_UTF_8;

   function Wide_Get_UTF_16
     (Buffer : in out Buffer_Type) return UTF_Encoding.UTF_16_Wide_String
   is
   begin
      return
        UTF_Encoding.Conversions.Convert
          (Get_UTF_8 (Buffer), Input_Scheme => UTF_Encoding.UTF_8);
   end Wide_Get_UTF_16;

   procedure Put_UTF_8_Implementation
     (Buffer : in out Root_Buffer_Type'Class;
      Item   : UTF_Encoding.UTF_8_String)
   is
      procedure Buffer_Type_Implementation (Buffer : in out Buffer_Type);
      --  View the passed-in Buffer parameter as being of type Buffer_Type,
      --  not of type Root_Buffer_Type'Class.

      procedure Buffer_Type_Implementation (Buffer : in out Buffer_Type) is
      begin
         for Char of Item loop

            --  The Trim_Leading_Space flag, which can be set prior to calling
            --  any of the Put operations, which will cause white space
            --  characters to be discarded by any Put operation until a
            --  non-white-space character is encountered, at which point
            --  the flag will be reset.

            if not Buffer.Trim_Leading_White_Spaces
              or else not Characters.Handling.Is_Space (Char)
            then
               Buffer.All_7_Bits := @ and then Character'Pos (Char) < 128;
               Buffer.Trim_Leading_White_Spaces := False;

               if Buffer.Last_Used = Buffer.List.Current_Chunk.Length then
                  --  Current chunk is full; allocate a new one with doubled
                  --  size

                  declare
                     Cc     : Chunk renames Buffer.List.Current_Chunk.all;
                     Max    : constant Positive := Integer'Last / 2;
                     Length : constant Natural  :=
                                Integer'Min (Max, 2 * Cc.Length);
                  begin
                     pragma Assert (Cc.Next = null);
                     Cc.Next                   := new Chunk (Length => Length);
                     Buffer.List.Current_Chunk := Cc.Next;
                     Buffer.Last_Used          := 0;
                  end;
               end if;

               Buffer.UTF_8_Length                                := @ + 1;
               Buffer.UTF_8_Column                                := @ + 1;
               Buffer.Last_Used                                   := @ + 1;
               Buffer.List.Current_Chunk.Chars (Buffer.Last_Used) := Char;
            end if;
         end loop;
      end Buffer_Type_Implementation;
   begin
      Buffer_Type_Implementation (Buffer_Type (Buffer));
   end Put_UTF_8_Implementation;

   procedure Initialize (List : in out Managed_Chunk_List) is
   begin
      List.Current_Chunk := List.First_Chunk'Unchecked_Access;
   end Initialize;

   procedure Finalize (List : in out Managed_Chunk_List) is
      procedure Free is new Ada.Unchecked_Deallocation (Chunk, Chunk_Access);
      Ptr : Chunk_Access := List.First_Chunk.Next;
   begin
      while Ptr /= null loop
         declare
            Old_Ptr : Chunk_Access := Ptr;
         begin
            Ptr := Ptr.Next;
            Free (Old_Ptr);
         end;
      end loop;

      List.First_Chunk.Next := null;
      Initialize (List);
   end Finalize;

end Ada.Strings.Text_Buffers.Unbounded;

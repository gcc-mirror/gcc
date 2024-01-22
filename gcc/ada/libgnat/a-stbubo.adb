------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                     ADA.STRINGS.TEXT_BUFFERS.BOUNDED                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2020-2024, Free Software Foundation, Inc.       --
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
with Ada.Strings.UTF_Encoding.Conversions;
with Ada.Strings.UTF_Encoding.Strings;
with Ada.Strings.UTF_Encoding.Wide_Strings;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
package body Ada.Strings.Text_Buffers.Bounded is

   --  Pretty much the same as the Unbounded version, except where different.
   --
   --  One could imagine inventing an Input_Mapping generic analogous to
   --  the existing Output_Mapping generic to address the Get-related
   --  Bounded/Unbounded code duplication issues, but let's not. In the
   --  Output case, there was more substantial duplication and there were
   --  3 clients (Bounded, Unbounded, and Files) instead of 2.

   function Text_Truncated (Buffer : Buffer_Type) return Boolean is
     (Buffer.Truncated);

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
      return
        Result : constant UTF_Encoding.UTF_8_String :=
          UTF_Encoding.UTF_8_String
            (Buffer.Chars (1 .. Text_Buffer_Count (Buffer.UTF_8_Length)))
      do
         --  Reset buffer to default initial value.
         declare
            Defaulted : Buffer_Type (0);

            --  If this aggregate becomes illegal due to new field, don't
            --  forget to add corresponding assignment statement below.
            Dummy : array (1 .. 0) of Buffer_Type (0) :=
              [others =>
                 (Max_Characters            => 0,
                  Chars                     => <>,
                  Indentation               => <>,
                  Indent_Pending            => <>,
                  UTF_8_Length              => <>,
                  UTF_8_Column              => <>,
                  Trim_Leading_White_Spaces => <>,
                  All_7_Bits                => <>,
                  All_8_Bits                => <>,
                  Truncated                 => <>)];
         begin
            Buffer.Indentation    := Defaulted.Indentation;
            Buffer.Indent_Pending := Defaulted.Indent_Pending;
            Buffer.UTF_8_Length   := Defaulted.UTF_8_Length;
            Buffer.UTF_8_Column   := Defaulted.UTF_8_Column;
            Buffer.All_7_Bits     := Defaulted.All_7_Bits;
            Buffer.All_8_Bits     := Defaulted.All_8_Bits;
            Buffer.Truncated      := Defaulted.Truncated;
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
      --  not of Root_Buffer_Type'Class.

      procedure Buffer_Type_Implementation (Buffer : in out Buffer_Type) is
      begin
         for Char of Item loop
            if Buffer.UTF_8_Length = Integer (Buffer.Max_Characters) then
               Buffer.Truncated := True;
               return;
            end if;

            Buffer.All_7_Bits :=
              @ and then Character'Pos (Char) < 128;
            Buffer.Trim_Leading_White_Spaces :=
              @ and then Characters.Handling.Is_Space (Char);

            Buffer.UTF_8_Length                                    := @ + 1;
            Buffer.UTF_8_Column                                    := @ + 1;
            Buffer.Chars (Text_Buffer_Count (Buffer.UTF_8_Length)) := Char;
         end loop;
      end Buffer_Type_Implementation;
   begin
      if Item'Length > 0 then
         Buffer_Type_Implementation (Buffer_Type (Buffer));
      end if;
   end Put_UTF_8_Implementation;

end Ada.Strings.Text_Buffers.Bounded;

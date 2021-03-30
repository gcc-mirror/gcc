------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                      ADA.STRINGS.TEXT_BUFFERS.UTILS                      --
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

package body Ada.Strings.Text_Buffers.Utils is

   procedure Put_7bit
     (Buffer : in out Root_Buffer_Type'Class; Item : Character_7)
   is
   begin
      Put (Buffer, (1 => Item));
   end Put_7bit;

   procedure Put_Character
     (Buffer : in out Root_Buffer_Type'Class; Item : Character)
   is
   begin
      Put (Buffer, (1 => Item));
   end Put_Character;

   procedure Put_Wide_Character
     (Buffer : in out Root_Buffer_Type'Class; Item : Wide_Character)
   is
   begin
      Wide_Put (Buffer, (1 => Item));
   end Put_Wide_Character;

   procedure Put_Wide_Wide_Character
     (Buffer : in out Root_Buffer_Type'Class; Item : Wide_Wide_Character)
   is
   begin
      Wide_Wide_Put (Buffer, (1 => Item));
   end Put_Wide_Wide_Character;

   procedure Put_UTF_8_Lines
     (Buffer : in out Root_Buffer_Type'Class; Item : UTF_8_Lines)
   is
   begin
      Put (Buffer, Item);
   end Put_UTF_8_Lines;

   function Column (Buffer : Root_Buffer_Type'Class) return Positive is
   begin
      return Buffer.UTF_8_Column;
   end Column;

   procedure Tab_To_Column
     (Buffer : in out Root_Buffer_Type'Class; Column : Positive)
   is
   begin
      Put (Buffer, String'(1 .. Column - Utils.Column (Buffer) => ' '));
   end Tab_To_Column;

end Ada.Strings.Text_Buffers.Utils;

------------------------------------------------------------------------------
--                                                                          --
--                        GNAT RUN-TIME COMPONENTS                          --
--                                                                          --
--              A D A . D I R E C T _ I O . C _ S T R E A M S               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2025, Free Software Foundation, Inc.         --
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

with Interfaces.C_Streams; use Interfaces.C_Streams;
with System.File_IO;
with System.File_Control_Block;
with System.Direct_IO;
with Ada.Unchecked_Conversion;

package body Ada.Direct_IO.C_Streams is

   package FIO renames System.File_IO;
   package FCB renames System.File_Control_Block;
   package DIO renames System.Direct_IO;

   subtype AP is FCB.AFCB_Ptr;

   function To_FCB is new Ada.Unchecked_Conversion (File_Mode, FCB.File_Mode);

   --------------
   -- C_Stream --
   --------------

   function C_Stream (F : File_Type) return FILEs is
   begin
      FIO.Check_File_Open (AP (F));
      return F.Stream;
   end C_Stream;

   ----------
   -- Open --
   ----------

   procedure Open
     (File     : in out File_Type;
      Mode     : File_Mode;
      C_Stream : FILEs;
      Form     : String := "";
      Name     : String := "")
   is
      Dummy_File_Control_Block : DIO.Direct_AFCB;
      pragma Warnings (Off, Dummy_File_Control_Block);
      --  Yes, we know this is never assigned a value, only the tag
      --  is used for dispatching purposes, so that's expected.

   begin
      FIO.Open (File_Ptr  => AP (File),
                Dummy_FCB => Dummy_File_Control_Block,
                Mode      => To_FCB (Mode),
                Name      => Name,
                Form      => Form,
                Amethod   => 'D',
                Creat     => False,
                Text      => False,
                C_Stream  => C_Stream);

      File.Bytes := Bytes;
   end Open;

end Ada.Direct_IO.C_Streams;

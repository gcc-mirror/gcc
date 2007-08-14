------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                     S Y S T E M . D I R E C T _ I O                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2007, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.IO_Exceptions;      use Ada.IO_Exceptions;
with Interfaces.C_Streams;   use Interfaces.C_Streams;
with System;                 use System;
with System.CRTL;
with System.File_IO;
with System.Soft_Links;
with Ada.Unchecked_Deallocation;

package body System.Direct_IO is

   package FIO renames System.File_IO;
   package SSL renames System.Soft_Links;

   subtype AP is FCB.AFCB_Ptr;
   use type FCB.Shared_Status_Type;

   use type System.CRTL.long;
   use type System.CRTL.size_t;

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Set_Position (File : File_Type);
   --  Sets file position pointer according to value of current index

   -------------------
   -- AFCB_Allocate --
   -------------------

   function AFCB_Allocate (Control_Block : Direct_AFCB) return FCB.AFCB_Ptr is
      pragma Unreferenced (Control_Block);

   begin
      return new Direct_AFCB;
   end AFCB_Allocate;

   ----------------
   -- AFCB_Close --
   ----------------

   --  No special processing required for Direct_IO close

   procedure AFCB_Close (File : not null access Direct_AFCB) is
      pragma Unreferenced (File);

   begin
      null;
   end AFCB_Close;

   ---------------
   -- AFCB_Free --
   ---------------

   procedure AFCB_Free (File : not null access Direct_AFCB) is

      type FCB_Ptr is access all Direct_AFCB;

      FT : FCB_Ptr := FCB_Ptr (File);

      procedure Free is new
        Ada.Unchecked_Deallocation (Direct_AFCB, FCB_Ptr);

   begin
      Free (FT);
   end AFCB_Free;

   ------------
   -- Create --
   ------------

   procedure Create
     (File : in out File_Type;
      Mode : FCB.File_Mode := FCB.Inout_File;
      Name : String := "";
      Form : String := "")
   is
      Dummy_File_Control_Block : Direct_AFCB;
      pragma Warnings (Off, Dummy_File_Control_Block);
      --  Yes, we know this is never assigned a value, only the tag
      --  is used for dispatching purposes, so that's expected.

   begin
      FIO.Open (File_Ptr  => AP (File),
                Dummy_FCB => Dummy_File_Control_Block,
                Mode      => Mode,
                Name      => Name,
                Form      => Form,
                Amethod   => 'D',
                Creat     => True,
                Text      => False);
   end Create;

   -----------------
   -- End_Of_File --
   -----------------

   function End_Of_File (File : File_Type) return Boolean is
   begin
      FIO.Check_Read_Status (AP (File));
      return Count (File.Index) > Size (File);
   end End_Of_File;

   -----------
   -- Index --
   -----------

   function Index (File : File_Type) return Positive_Count is
   begin
      FIO.Check_File_Open (AP (File));
      return Count (File.Index);
   end Index;

   ----------
   -- Open --
   ----------

   procedure Open
     (File : in out File_Type;
      Mode : FCB.File_Mode;
      Name : String;
      Form : String := "")
   is
      Dummy_File_Control_Block : Direct_AFCB;
      pragma Warnings (Off, Dummy_File_Control_Block);
      --  Yes, we know this is never assigned a value, only the tag
      --  is used for dispatching purposes, so that's expected.

   begin
      FIO.Open (File_Ptr  => AP (File),
                Dummy_FCB => Dummy_File_Control_Block,
                Mode      => Mode,
                Name      => Name,
                Form      => Form,
                Amethod   => 'D',
                Creat     => False,
                Text      => False);
   end Open;

   ----------
   -- Read --
   ----------

   procedure Read
     (File : File_Type;
      Item : Address;
      Size : Interfaces.C_Streams.size_t;
      From : Positive_Count)
   is
   begin
      Set_Index (File, From);
      Read (File, Item, Size);
   end Read;

   procedure Read
     (File : File_Type;
      Item : Address;
      Size : Interfaces.C_Streams.size_t)
   is
   begin
      FIO.Check_Read_Status (AP (File));

      --  If last operation was not a read, or if in file sharing mode,
      --  then reset the physical pointer of the file to match the index
      --  We lock out task access over the two operations in this case.

      if File.Last_Op /= Op_Read
        or else File.Shared_Status = FCB.Yes
      then
         if End_Of_File (File) then
            raise End_Error;
         end if;

         Locked_Processing : begin
            SSL.Lock_Task.all;
            Set_Position (File);
            FIO.Read_Buf (AP (File), Item, Size);
            SSL.Unlock_Task.all;

         exception
            when others =>
               SSL.Unlock_Task.all;
               raise;
         end Locked_Processing;

      else
         FIO.Read_Buf (AP (File), Item, Size);
      end if;

      File.Index := File.Index + 1;

      --  Set last operation to read, unless we did not read a full record
      --  (happens with the variant record case) in which case we set the
      --  last operation as other, to force the file position to be reset
      --  on the next read.

      if File.Bytes = Size then
         File.Last_Op := Op_Read;
      else
         File.Last_Op := Op_Other;
      end if;
   end Read;

   --  The following is the required overriding for Stream.Read, which is
   --  not used, since we do not do Stream operations on Direct_IO files.

   procedure Read
     (File : in out Direct_AFCB;
      Item : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset)
   is
   begin
      raise Program_Error;
   end Read;

   -----------
   -- Reset --
   -----------

   procedure Reset (File : in out File_Type; Mode : FCB.File_Mode) is
   begin
      FIO.Reset (AP (File), Mode);
      File.Index := 1;
      File.Last_Op := Op_Read;
   end Reset;

   procedure Reset (File : in out File_Type) is
   begin
      FIO.Reset (AP (File));
      File.Index := 1;
      File.Last_Op := Op_Read;
   end Reset;

   ---------------
   -- Set_Index --
   ---------------

   procedure Set_Index (File : File_Type; To : Positive_Count) is
   begin
      FIO.Check_File_Open (AP (File));
      File.Index := Count (To);
      File.Last_Op := Op_Other;
   end Set_Index;

   ------------------
   -- Set_Position --
   ------------------

   procedure Set_Position (File : File_Type) is
   begin
      if fseek
           (File.Stream, long (File.Bytes) *
              long (File.Index - 1), SEEK_SET) /= 0
      then
         raise Use_Error;
      end if;
   end Set_Position;

   ----------
   -- Size --
   ----------

   function Size (File : File_Type) return Count is
   begin
      FIO.Check_File_Open (AP (File));
      File.Last_Op := Op_Other;

      if fseek (File.Stream, 0, SEEK_END) /= 0 then
         raise Device_Error;
      end if;

      return Count (ftell (File.Stream) / long (File.Bytes));
   end Size;

   -----------
   -- Write --
   -----------

   procedure Write
     (File   : File_Type;
      Item   : Address;
      Size   : Interfaces.C_Streams.size_t;
      Zeroes : System.Storage_Elements.Storage_Array)

   is
      procedure Do_Write;
      --  Do the actual write

      --------------
      -- Do_Write --
      --------------

      procedure Do_Write is
      begin
         FIO.Write_Buf (AP (File), Item, Size);

         --  If we did not write the whole record (happens with the variant
         --  record case), then fill out the rest of the record with zeroes.
         --  This is cleaner in any case, and is required for the last
         --  record, since otherwise the length of the file is wrong.

         if File.Bytes > Size then
            FIO.Write_Buf (AP (File), Zeroes'Address, File.Bytes - Size);
         end if;
      end Do_Write;

   --  Start of processing for Write

   begin
      FIO.Check_Write_Status (AP (File));

      --  If last operation was not a write, or if in file sharing mode,
      --  then reset the physical pointer of the file to match the index
      --  We lock out task access over the two operations in this case.

      if File.Last_Op /= Op_Write
        or else File.Shared_Status = FCB.Yes
      then
         Locked_Processing : begin
            SSL.Lock_Task.all;
            Set_Position (File);
            Do_Write;
            SSL.Unlock_Task.all;

         exception
            when others =>
               SSL.Unlock_Task.all;
               raise;
         end Locked_Processing;

      else
         Do_Write;
      end if;

      File.Index := File.Index + 1;

      --  Set last operation to write, unless we did not read a full record
      --  (happens with the variant record case) in which case we set the
      --  last operation as other, to force the file position to be reset
      --  on the next write.

      if File.Bytes = Size then
         File.Last_Op := Op_Write;
      else
         File.Last_Op := Op_Other;
      end if;
   end Write;

   --  The following is the required overriding for Stream.Write, which is
   --  not used, since we do not do Stream operations on Direct_IO files.

   procedure Write
     (File : in out Direct_AFCB;
      Item : Ada.Streams.Stream_Element_Array)
   is
   begin
      raise Program_Error;
   end Write;

end System.Direct_IO;

------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                A D A . S T R E A M S . S T R E A M _ I O                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.1 $
--                                                                          --
--          Copyright (C) 1992-2000, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
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

with Interfaces.C_Streams;      use Interfaces.C_Streams;
with System;                    use System;
with System.File_IO;
with System.Soft_Links;
with Unchecked_Conversion;
with Unchecked_Deallocation;

package body Ada.Streams.Stream_IO is

   package FIO renames System.File_IO;
   package SSL renames System.Soft_Links;

   subtype AP is FCB.AFCB_Ptr;

   function To_FCB is new Unchecked_Conversion (File_Mode, FCB.File_Mode);
   function To_SIO is new Unchecked_Conversion (FCB.File_Mode, File_Mode);
   use type FCB.File_Mode;
   use type FCB.Shared_Status_Type;

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Set_Position (File : in File_Type);
   --  Sets file position pointer according to value of current index

   -------------------
   -- AFCB_Allocate --
   -------------------

   function AFCB_Allocate (Control_Block : Stream_AFCB) return FCB.AFCB_Ptr is
   begin
      return new Stream_AFCB;
   end AFCB_Allocate;

   ----------------
   -- AFCB_Close --
   ----------------

   --  No special processing required for closing Stream_IO file

   procedure AFCB_Close (File : access Stream_AFCB) is
   begin
      null;
   end AFCB_Close;

   ---------------
   -- AFCB_Free --
   ---------------

   procedure AFCB_Free (File : access Stream_AFCB) is
      type FCB_Ptr is access all Stream_AFCB;
      FT : FCB_Ptr := FCB_Ptr (File);

      procedure Free is new Unchecked_Deallocation (Stream_AFCB, FCB_Ptr);

   begin
      Free (FT);
   end AFCB_Free;

   -----------
   -- Close --
   -----------

   procedure Close (File : in out File_Type) is
   begin
      FIO.Close (AP (File));
   end Close;

   ------------
   -- Create --
   ------------

   procedure Create
     (File : in out File_Type;
      Mode : in File_Mode := Out_File;
      Name : in String := "";
      Form : in String := "")
   is
      File_Control_Block : Stream_AFCB;

   begin
      FIO.Open (File_Ptr  => AP (File),
                Dummy_FCB => File_Control_Block,
                Mode      => To_FCB (Mode),
                Name      => Name,
                Form      => Form,
                Amethod   => 'S',
                Creat     => True,
                Text      => False);
      File.Last_Op := Op_Write;
   end Create;

   ------------
   -- Delete --
   ------------

   procedure Delete (File : in out File_Type) is
   begin
      FIO.Delete (AP (File));
   end Delete;

   -----------------
   -- End_Of_File --
   -----------------

   function End_Of_File (File : in File_Type) return Boolean is
   begin
      FIO.Check_Read_Status (AP (File));
      return Count (File.Index) > Size (File);
   end End_Of_File;

   -----------
   -- Flush --
   -----------

   procedure Flush (File : in out File_Type) is
   begin
      FIO.Flush (AP (File));
   end Flush;

   ----------
   -- Form --
   ----------

   function Form (File : in File_Type) return String is
   begin
      return FIO.Form (AP (File));
   end Form;

   -----------
   -- Index --
   -----------

   function Index (File : in File_Type) return Positive_Count is
   begin
      FIO.Check_File_Open (AP (File));
      return Count (File.Index);
   end Index;

   -------------
   -- Is_Open --
   -------------

   function Is_Open (File : in File_Type) return Boolean is
   begin
      return FIO.Is_Open (AP (File));
   end Is_Open;

   ----------
   -- Mode --
   ----------

   function Mode (File : in File_Type) return File_Mode is
   begin
      return To_SIO (FIO.Mode (AP (File)));
   end Mode;

   ----------
   -- Name --
   ----------

   function Name (File : in File_Type) return String is
   begin
      return FIO.Name (AP (File));
   end Name;

   ----------
   -- Open --
   ----------

   procedure Open
     (File : in out File_Type;
      Mode : in File_Mode;
      Name : in String;
      Form : in String := "")
   is
      File_Control_Block : Stream_AFCB;

   begin
      FIO.Open (File_Ptr  => AP (File),
                Dummy_FCB => File_Control_Block,
                Mode      => To_FCB (Mode),
                Name      => Name,
                Form      => Form,
                Amethod   => 'S',
                Creat     => False,
                Text      => False);

      --  Ensure that the stream index is set properly (e.g., for Append_File)

      Reset (File, Mode);

      File.Last_Op := Op_Read;
   end Open;

   ----------
   -- Read --
   ----------

   procedure Read
     (File : in File_Type;
      Item : out Stream_Element_Array;
      Last : out Stream_Element_Offset;
      From : in Positive_Count)
   is
   begin
      Set_Index (File, From);
      Read (File, Item, Last);
   end Read;

   procedure Read
     (File : in File_Type;
      Item : out Stream_Element_Array;
      Last : out Stream_Element_Offset)
   is
      Nread : size_t;

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
            FIO.Read_Buf (AP (File), Item'Address, Item'Length, Nread);
            SSL.Unlock_Task.all;

         exception
            when others =>
               SSL.Unlock_Task.all;
               raise;
         end Locked_Processing;

      else
         FIO.Read_Buf (AP (File), Item'Address, Item'Length, Nread);
      end if;

      File.Index := File.Index + Count (Nread);
      Last := Item'First + Stream_Element_Offset (Nread) - 1;
      File.Last_Op := Op_Read;
   end Read;

   --  This version of Read is the primitive operation on the underlying
   --  Stream type, used when a Stream_IO file is treated as a Stream

   procedure Read
     (File : in out Stream_AFCB;
      Item : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset)
   is
   begin
      Read (File'Unchecked_Access, Item, Last);
   end Read;

   -----------
   -- Reset --
   -----------

   procedure Reset (File : in out File_Type; Mode : in File_Mode) is
   begin
      FIO.Check_File_Open (AP (File));

      --  Reset file index to start of file for read/write cases. For
      --  the append case, the Set_Mode call repositions the index.

      File.Index := 1;
      Set_Mode (File, Mode);
   end Reset;

   procedure Reset (File : in out File_Type) is
   begin
      Reset (File, To_SIO (File.Mode));
   end Reset;

   ---------------
   -- Set_Index --
   ---------------

   procedure Set_Index (File : in File_Type; To : in Positive_Count) is
   begin
      FIO.Check_File_Open (AP (File));
      File.Index := Count (To);
      File.Last_Op := Op_Other;
   end Set_Index;

   --------------
   -- Set_Mode --
   --------------

   procedure Set_Mode (File : in out File_Type; Mode : in File_Mode) is
   begin
      FIO.Check_File_Open (AP (File));

      --  If we are switching from read to write, or vice versa, and
      --  we are not already open in update mode, then reopen in update
      --  mode now. Note that we can use Inout_File as the mode for the
      --  call since File_IO handles all modes for all file types.

      if ((File.Mode = FCB.In_File) /= (Mode = In_File))
        and then not File.Update_Mode
      then
         FIO.Reset (AP (File), FCB.Inout_File);
         File.Update_Mode := True;
      end if;

      --  Set required mode and position to end of file if append mode

      File.Mode := To_FCB (Mode);
      FIO.Append_Set (AP (File));

      if File.Mode = FCB.Append_File then
         File.Index := Count (ftell (File.Stream)) + 1;
      end if;

      File.Last_Op := Op_Other;
   end Set_Mode;

   ------------------
   -- Set_Position --
   ------------------

   procedure Set_Position (File : in File_Type) is
   begin
      if fseek (File.Stream, long (File.Index) - 1, SEEK_SET) /= 0 then
         raise Use_Error;
      end if;
   end Set_Position;

   ----------
   -- Size --
   ----------

   function Size (File : in File_Type) return Count is
   begin
      FIO.Check_File_Open (AP (File));

      if File.File_Size = -1 then
         File.Last_Op := Op_Other;

         if fseek (File.Stream, 0, SEEK_END) /= 0 then
            raise Device_Error;
         end if;

         File.File_Size := Stream_Element_Offset (ftell (File.Stream));
      end if;

      return Count (File.File_Size);
   end Size;

   ------------
   -- Stream --
   ------------

   function Stream (File : in File_Type) return Stream_Access is
   begin
      FIO.Check_File_Open (AP (File));
      return Stream_Access (File);
   end Stream;

   -----------
   -- Write --
   -----------

   procedure Write
     (File : in File_Type;
      Item : in Stream_Element_Array;
      To   : in Positive_Count)
   is
   begin
      Set_Index (File, To);
      Write (File, Item);
   end Write;

   procedure Write (File : in File_Type; Item : in Stream_Element_Array) is
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
            FIO.Write_Buf (AP (File), Item'Address, Item'Length);
            SSL.Unlock_Task.all;

         exception
            when others =>
               SSL.Unlock_Task.all;
               raise;
         end Locked_Processing;

      else
         FIO.Write_Buf (AP (File), Item'Address, Item'Length);
      end if;

      File.Index := File.Index + Item'Length;
      File.Last_Op := Op_Write;
      File.File_Size := -1;
   end Write;

   --  This version of Write is the primitive operation on the underlying
   --  Stream type, used when a Stream_IO file is treated as a Stream

   procedure Write
     (File : in out Stream_AFCB;
      Item : in Ada.Streams.Stream_Element_Array)
   is
   begin
      Write (File'Unchecked_Access, Item);
   end Write;

end Ada.Streams.Stream_IO;

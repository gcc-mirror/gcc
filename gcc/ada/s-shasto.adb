------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                 S Y S T E M . S H A R E D _ M E M O R Y                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1998-2014, Free Software Foundation, Inc.         --
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

with Ada.IO_Exceptions;
with Ada.Streams;
with Ada.Streams.Stream_IO;

with System.Global_Locks;
with System.Soft_Links;

with System;
with System.File_Control_Block;
with System.File_IO;
with System.HTable;

with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;

package body System.Shared_Storage is

   package AS renames Ada.Streams;

   package IOX renames Ada.IO_Exceptions;

   package FCB renames System.File_Control_Block;

   package SFI renames System.File_IO;

   package SIO renames Ada.Streams.Stream_IO;

   type String_Access is access String;
   procedure Free is new Ada.Unchecked_Deallocation
     (Object => String, Name => String_Access);

   Dir : String_Access;
   --  Holds the directory

   ------------------------------------------------
   -- Variables for Shared Variable Access Files --
   ------------------------------------------------

   Max_Shared_Var_Files : constant := 20;
   --  Maximum number of lock files that can be open

   Shared_Var_Files_Open : Natural := 0;
   --  Number of shared variable access files currently open

   type File_Stream_Type is new AS.Root_Stream_Type with record
      File : SIO.File_Type;
   end record;
   type File_Stream_Access is access all File_Stream_Type'Class;

   procedure Read
     (Stream : in out File_Stream_Type;
      Item   : out AS.Stream_Element_Array;
      Last   : out AS.Stream_Element_Offset);

   procedure Write
     (Stream : in out File_Stream_Type;
      Item   : AS.Stream_Element_Array);

   subtype Hash_Header is Natural range 0 .. 30;
   --  Number of hash headers, related (for efficiency purposes only) to the
   --  maximum number of lock files.

   type Shared_Var_File_Entry;
   type Shared_Var_File_Entry_Ptr is access Shared_Var_File_Entry;

   type Shared_Var_File_Entry is record
      Name : String_Access;
      --  Name of variable, as passed to Read_File/Write_File routines

      Stream : File_Stream_Access;
      --  Stream_IO file for the shared variable file

      Next : Shared_Var_File_Entry_Ptr;
      Prev : Shared_Var_File_Entry_Ptr;
      --  Links for LRU chain
   end record;

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Shared_Var_File_Entry,
      Name   => Shared_Var_File_Entry_Ptr);

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => File_Stream_Type'Class,
      Name   => File_Stream_Access);

   function To_AFCB_Ptr is
     new Ada.Unchecked_Conversion (SIO.File_Type, FCB.AFCB_Ptr);

   LRU_Head : Shared_Var_File_Entry_Ptr;
   LRU_Tail : Shared_Var_File_Entry_Ptr;
   --  As lock files are opened, they are organized into a least recently
   --  used chain, which is a doubly linked list using the Next and Prev
   --  fields of Shared_Var_File_Entry records. The field LRU_Head points
   --  to the least recently used entry, whose prev pointer is null, and
   --  LRU_Tail points to the most recently used entry, whose next pointer
   --  is null. These pointers are null only if the list is empty.

   function Hash  (F : String_Access)      return Hash_Header;
   function Equal (F1, F2 : String_Access) return Boolean;
   --  Hash and equality functions for hash table

   package SFT is new System.HTable.Simple_HTable
     (Header_Num => Hash_Header,
      Element    => Shared_Var_File_Entry_Ptr,
      No_Element => null,
      Key        => String_Access,
      Hash       => Hash,
      Equal      => Equal);

   --------------------------------
   -- Variables for Lock Control --
   --------------------------------

   Global_Lock : Global_Locks.Lock_Type;

   Lock_Count : Natural := 0;
   --  Counts nesting of lock calls, 0 means lock is not held

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Initialize;
   --  Called to initialize data structures for this package.
   --  Has no effect except on the first call.

   procedure Enter_SFE (SFE : Shared_Var_File_Entry_Ptr; Fname : String);
   --  The first parameter is a pointer to a newly allocated SFE, whose
   --  File field is already set appropriately. Fname is the name of the
   --  variable as passed to Shared_Var_RFile/Shared_Var_WFile. Enter_SFE
   --  completes the SFE value, and enters it into the hash table. If the
   --  hash table is already full, the least recently used entry is first
   --  closed and discarded.

   function Retrieve (File : String) return Shared_Var_File_Entry_Ptr;
   --  Given a file name, this function searches the hash table to see if
   --  the file is currently open. If so, then a pointer to the already
   --  created entry is returned, after first moving it to the head of
   --  the LRU chain. If not, then null is returned.

   function Shared_Var_ROpen (Var : String) return SIO.Stream_Access;
   --  As described above, this routine returns null if the
   --  corresponding shared storage does not exist, and otherwise, if
   --  the storage does exist, a Stream_Access value that references
   --  the shared storage, ready to read the current value.

   function Shared_Var_WOpen (Var : String) return SIO.Stream_Access;
   --  As described above, this routine returns a Stream_Access value
   --  that references the shared storage, ready to write the new
   --  value. The storage is created by this call if it does not
   --  already exist.

   procedure Shared_Var_Close (Var : SIO.Stream_Access);
   --  This routine signals the end of a read/assign operation. It can
   --  be useful to embrace a read/write operation between a call to
   --  open and a call to close which protect the whole operation.
   --  Otherwise, two simultaneous operations can result in the
   --  raising of exception Data_Error by setting the access mode of
   --  the variable in an incorrect mode.

   ---------------
   -- Enter_SFE --
   ---------------

   procedure Enter_SFE (SFE : Shared_Var_File_Entry_Ptr; Fname : String) is
      Freed : Shared_Var_File_Entry_Ptr;

   begin
      SFE.Name := new String'(Fname);

      --  Release least recently used entry if we have to

      if Shared_Var_Files_Open =  Max_Shared_Var_Files then
         Freed := LRU_Head;

         if Freed.Next /= null then
            Freed.Next.Prev := null;
         end if;

         LRU_Head := Freed.Next;
         SFT.Remove (Freed.Name);
         SIO.Close (Freed.Stream.File);
         Free (Freed.Name);
         Free (Freed.Stream);
         Free (Freed);

      else
         Shared_Var_Files_Open := Shared_Var_Files_Open + 1;
      end if;

      --  Add new entry to hash table

      SFT.Set (SFE.Name, SFE);

      --  Add new entry at end of LRU chain

      if LRU_Head = null then
         LRU_Head := SFE;
         LRU_Tail := SFE;

      else
         SFE.Prev := LRU_Tail;
         LRU_Tail.Next := SFE;
         LRU_Tail := SFE;
      end if;
   end Enter_SFE;

   -----------
   -- Equal --
   -----------

   function Equal (F1, F2 : String_Access) return Boolean is
   begin
      return F1.all = F2.all;
   end Equal;

   ----------
   -- Hash --
   ----------

   function Hash (F : String_Access) return Hash_Header is
      N : Natural := 0;

   begin
      --  Add up characters of name, mod our table size

      for J in F'Range loop
         N := (N + Character'Pos (F (J))) mod (Hash_Header'Last + 1);
      end loop;

      return N;
   end Hash;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      procedure Get_Env_Value_Ptr (Name, Length, Ptr : Address);
      pragma Import (C, Get_Env_Value_Ptr, "__gnat_getenv");

      procedure Strncpy (Astring_Addr, Cstring : Address; N : Integer);
      pragma Import (C, Strncpy, "strncpy");

      Dir_Name : aliased constant String :=
                   "SHARED_MEMORY_DIRECTORY" & ASCII.NUL;

      Env_Value_Ptr    : aliased Address;
      Env_Value_Length : aliased Integer;

   begin
      if Dir = null then
         Get_Env_Value_Ptr
           (Dir_Name'Address, Env_Value_Length'Address, Env_Value_Ptr'Address);

         Dir := new String (1 .. Env_Value_Length);

         if Env_Value_Length > 0 then
            Strncpy (Dir.all'Address, Env_Value_Ptr, Env_Value_Length);
         end if;

         System.Global_Locks.Create_Lock (Global_Lock, Dir.all & "__lock");
      end if;
   end Initialize;

   ----------
   -- Read --
   ----------

   procedure Read
     (Stream : in out File_Stream_Type;
      Item   : out AS.Stream_Element_Array;
      Last   : out AS.Stream_Element_Offset)
   is
   begin
      SIO.Read (Stream.File, Item, Last);

   exception when others =>
      Last := Item'Last;
   end Read;

   --------------
   -- Retrieve --
   --------------

   function Retrieve (File : String) return Shared_Var_File_Entry_Ptr is
      SFE : Shared_Var_File_Entry_Ptr;

   begin
      Initialize;
      SFE := SFT.Get (File'Unrestricted_Access);

      if SFE /= null then

         --  Move to head of LRU chain

         if SFE = LRU_Tail then
            null;

         elsif SFE = LRU_Head then
            LRU_Head := LRU_Head.Next;
            LRU_Head.Prev := null;

         else
            SFE.Next.Prev := SFE.Prev;
            SFE.Prev.Next := SFE.Next;
         end if;

         SFE.Next := null;
         SFE.Prev := LRU_Tail;
         LRU_Tail.Next := SFE;
         LRU_Tail := SFE;
      end if;

      return SFE;
   end Retrieve;

   ----------------------
   -- Shared_Var_Close --
   ----------------------

   procedure Shared_Var_Close (Var : SIO.Stream_Access) is
      pragma Warnings (Off, Var);

   begin
      System.Soft_Links.Unlock_Task.all;
   end Shared_Var_Close;

   ---------------------
   -- Shared_Var_Lock --
   ---------------------

   procedure Shared_Var_Lock (Var : String) is
      pragma Warnings (Off, Var);

   begin
      System.Soft_Links.Lock_Task.all;
      Initialize;

      if Lock_Count /= 0 then
         Lock_Count := Lock_Count + 1;
         System.Soft_Links.Unlock_Task.all;

      else
         Lock_Count := 1;
         System.Soft_Links.Unlock_Task.all;
         System.Global_Locks.Acquire_Lock (Global_Lock);
      end if;

   exception
      when others =>
         System.Soft_Links.Unlock_Task.all;
         raise;
   end Shared_Var_Lock;

   ----------------------
   -- Shared_Var_Procs --
   ----------------------

   package body Shared_Var_Procs is

      use type SIO.Stream_Access;

      ----------
      -- Read --
      ----------

      procedure Read is
         S : SIO.Stream_Access := null;
      begin
         S := Shared_Var_ROpen (Full_Name);
         if S /= null then
            Typ'Read (S, V);
            Shared_Var_Close (S);
         end if;
      end Read;

      ------------
      -- Write --
      ------------

      procedure Write is
         S : SIO.Stream_Access := null;
      begin
         S := Shared_Var_WOpen (Full_Name);
         Typ'Write (S, V);
         Shared_Var_Close (S);
         return;
      end Write;

   end Shared_Var_Procs;

   ----------------------
   -- Shared_Var_ROpen --
   ----------------------

   function Shared_Var_ROpen (Var : String) return SIO.Stream_Access is
      SFE : Shared_Var_File_Entry_Ptr;

      use type Ada.Streams.Stream_IO.File_Mode;

   begin
      System.Soft_Links.Lock_Task.all;
      SFE := Retrieve (Var);

      --  Here if file is not already open, try to open it

      if SFE = null then
         declare
            S  : aliased constant String := Dir.all & Var;

         begin
            SFE := new Shared_Var_File_Entry;
            SFE.Stream := new File_Stream_Type;
            SIO.Open (SFE.Stream.File, SIO.In_File, Name => S);
            SFI.Make_Unbuffered (To_AFCB_Ptr (SFE.Stream.File));

            --  File opened successfully, put new entry in hash table. Note
            --  that in this case, file is positioned correctly for read.

            Enter_SFE (SFE, Var);

            exception
               --  If we get an exception, it means that the file does not
               --  exist, and in this case, we don't need the SFE and we
               --  return null;

               when IOX.Name_Error =>
                  Free (SFE);
                  System.Soft_Links.Unlock_Task.all;
                  return null;
         end;

      --  Here if file is already open, set file for reading

      else
         if SIO.Mode (SFE.Stream.File) /= SIO.In_File then
            SIO.Set_Mode (SFE.Stream.File, SIO.In_File);
            SFI.Make_Unbuffered (To_AFCB_Ptr (SFE.Stream.File));
         end if;

         SIO.Set_Index (SFE.Stream.File, 1);
      end if;

      return SIO.Stream_Access (SFE.Stream);

   exception
      when others =>
         System.Soft_Links.Unlock_Task.all;
         raise;
   end Shared_Var_ROpen;

   -----------------------
   -- Shared_Var_Unlock --
   -----------------------

   procedure Shared_Var_Unlock (Var : String) is
      pragma Warnings (Off, Var);

   begin
      System.Soft_Links.Lock_Task.all;
      Initialize;
      Lock_Count := Lock_Count - 1;

      if Lock_Count = 0 then
         System.Global_Locks.Release_Lock (Global_Lock);
      end if;
      System.Soft_Links.Unlock_Task.all;

   exception
      when others =>
         System.Soft_Links.Unlock_Task.all;
         raise;
   end Shared_Var_Unlock;

   ---------------------
   -- Share_Var_WOpen --
   ---------------------

   function Shared_Var_WOpen (Var : String) return SIO.Stream_Access is
      SFE : Shared_Var_File_Entry_Ptr;

      use type Ada.Streams.Stream_IO.File_Mode;

   begin
      System.Soft_Links.Lock_Task.all;
      SFE := Retrieve (Var);

      if SFE = null then
         declare
            S  : aliased constant String := Dir.all & Var;

         begin
            SFE := new Shared_Var_File_Entry;
            SFE.Stream := new File_Stream_Type;
            SIO.Open (SFE.Stream.File, SIO.Out_File, Name => S);
            SFI.Make_Unbuffered (To_AFCB_Ptr (SFE.Stream.File));

         exception
            --  If we get an exception, it means that the file does not
            --  exist, and in this case, we create the file.

            when IOX.Name_Error =>

               begin
                  SIO.Create (SFE.Stream.File, SIO.Out_File, Name => S);

               exception
                  --  Error if we cannot create the file

                  when others =>
                     raise Program_Error with
                        "cannot create shared variable file for """ & S & '"';
               end;
         end;

         --  Make new hash table entry for opened/created file. Note that
         --  in both cases, the file is already in write mode at the start
         --  of the file, ready to be written.

         Enter_SFE (SFE, Var);

      --  Here if file is already open, set file for writing

      else
         if SIO.Mode (SFE.Stream.File) /= SIO.Out_File then
            SIO.Set_Mode (SFE.Stream.File, SIO.Out_File);
            SFI.Make_Unbuffered (To_AFCB_Ptr (SFE.Stream.File));
         end if;

         SIO.Set_Index (SFE.Stream.File, 1);
      end if;

      return SIO.Stream_Access (SFE.Stream);

   exception
      when others =>
         System.Soft_Links.Unlock_Task.all;
         raise;
   end Shared_Var_WOpen;

   -----------
   -- Write --
   -----------

   procedure Write
     (Stream : in out File_Stream_Type;
      Item   : AS.Stream_Element_Array)
   is
   begin
      SIO.Write (Stream.File, Item);
   end Write;

end System.Shared_Storage;

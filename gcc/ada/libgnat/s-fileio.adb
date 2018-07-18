------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . F I L E _ I O                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2018, Free Software Foundation, Inc.         --
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

with Ada.Finalization;           use Ada.Finalization;
with Ada.IO_Exceptions;          use Ada.IO_Exceptions;
with Ada.Unchecked_Deallocation;

with Interfaces.C_Streams;       use Interfaces.C_Streams;

with System.Case_Util;           use System.Case_Util;
with System.CRTL;
with System.OS_Lib;
with System.Soft_Links;

package body System.File_IO is

   use System.File_Control_Block;

   package SSL renames System.Soft_Links;

   use type CRTL.size_t;

   ----------------------
   -- Global Variables --
   ----------------------

   Open_Files : AFCB_Ptr;
   --  This points to a list of AFCB's for all open files. This is a doubly
   --  linked list, with the Prev pointer of the first entry, and the Next
   --  pointer of the last entry containing null. Note that this global
   --  variable must be properly protected to provide thread safety.

   type Temp_File_Record;
   type Temp_File_Record_Ptr is access all Temp_File_Record;

   type Temp_File_Record is record
      File : AFCB_Ptr;
      Next : aliased Temp_File_Record_Ptr;
      Name : String (1 .. max_path_len + 1);
   end record;
   --  One of these is allocated for each temporary file created

   Temp_Files : aliased Temp_File_Record_Ptr;
   --  Points to list of names of temporary files. Note that this global
   --  variable must be properly protected to provide thread safety.

   procedure Free is new Ada.Unchecked_Deallocation
     (Temp_File_Record, Temp_File_Record_Ptr);

   type File_IO_Clean_Up_Type is new Limited_Controlled with null record;
   --  The closing of all open files and deletion of temporary files is an
   --  action that takes place at the end of execution of the main program.
   --  This action is implemented using a library level object that gets
   --  finalized at the end of program execution. Note that the type is
   --  limited, in order to stop the compiler optimizing away the declaration
   --  which would be allowed in the non-limited case.

   procedure Finalize (V : in out File_IO_Clean_Up_Type);
   --  This is the finalize operation that is used to do the cleanup

   File_IO_Clean_Up_Object : File_IO_Clean_Up_Type;
   pragma Warnings (Off, File_IO_Clean_Up_Object);
   --  This is the single object of the type that triggers the finalization
   --  call. Since it is at the library level, this happens just before the
   --  environment task is finalized.

   text_translation_required : Boolean;
   for text_translation_required'Size use Character'Size;
   pragma Import
     (C, text_translation_required, "__gnat_text_translation_required");
   --  If true, add appropriate suffix to control string for Open

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Free_String is new Ada.Unchecked_Deallocation (String, Pstring);

   subtype Fopen_String is String (1 .. 4);
   --  Holds open string (longest is "w+b" & nul)

   procedure Fopen_Mode
     (Namestr : String;
      Mode    : File_Mode;
      Text    : Boolean;
      Creat   : Boolean;
      Amethod : Character;
      Fopstr  : out Fopen_String);
   --  Determines proper open mode for a file to be opened in the given Ada
   --  mode. Namestr is the NUL-terminated file name. Text is true for a text
   --  file and false otherwise, and Creat is true for a create call, and False
   --  for an open call. The value stored in Fopstr is a nul-terminated string
   --  suitable for a call to fopen or freopen. Amethod is the character
   --  designating the access method from the Access_Method field of the FCB.

   function Errno_Message
     (Name  : String;
      Errno : Integer := OS_Lib.Errno) return String;
   --  Return Errno_Message for Errno, with file name prepended

   procedure Raise_Device_Error
     (File  : AFCB_Ptr;
      Errno : Integer := OS_Lib.Errno);
   pragma No_Return (Raise_Device_Error);
   --  Clear error indication on File and raise Device_Error with an exception
   --  message providing errno information.

   ----------------
   -- Append_Set --
   ----------------

   procedure Append_Set (File : AFCB_Ptr) is
   begin
      if File.Mode = Append_File then
         if fseek (File.Stream, 0, SEEK_END) /= 0 then
            Raise_Device_Error (File);
         end if;
      end if;
   end Append_Set;

   ----------------
   -- Chain_File --
   ----------------

   procedure Chain_File (File : AFCB_Ptr) is
   begin
      --  Take a task lock, to protect the global data value Open_Files

      SSL.Lock_Task.all;

      --  Do the chaining operation locked

      File.Next := Open_Files;
      File.Prev := null;
      Open_Files := File;

      if File.Next /= null then
         File.Next.Prev := File;
      end if;

      SSL.Unlock_Task.all;

   exception
      when others =>
         SSL.Unlock_Task.all;
         raise;
   end Chain_File;

   ---------------------
   -- Check_File_Open --
   ---------------------

   procedure Check_File_Open (File : AFCB_Ptr) is
   begin
      if File = null then
         raise Status_Error with "file not open";
      end if;
   end Check_File_Open;

   -----------------------
   -- Check_Read_Status --
   -----------------------

   procedure Check_Read_Status (File : AFCB_Ptr) is
   begin
      if File = null then
         raise Status_Error with "file not open";
      elsif File.Mode not in Read_File_Mode then
         raise Mode_Error with "file not readable";
      end if;
   end Check_Read_Status;

   ------------------------
   -- Check_Write_Status --
   ------------------------

   procedure Check_Write_Status (File : AFCB_Ptr) is
   begin
      if File = null then
         raise Status_Error with "file not open";
      elsif File.Mode = In_File then
         raise Mode_Error with "file not writable";
      end if;
   end Check_Write_Status;

   -----------
   -- Close --
   -----------

   procedure Close (File_Ptr : access AFCB_Ptr) is
      Close_Status : int     := 0;
      Dup_Strm     : Boolean := False;
      Errno        : Integer := 0;

      File : AFCB_Ptr renames File_Ptr.all;

   begin
      --  Take a task lock, to protect the global variables Open_Files and
      --  Temp_Files, and the chains they point to.

      SSL.Lock_Task.all;

      Check_File_Open (File);
      AFCB_Close (File);

      --  Sever the association between the given file and its associated
      --  external file. The given file is left closed. Do not perform system
      --  closes on the standard input, output and error files and also do not
      --  attempt to close a stream that does not exist (signalled by a null
      --  stream value -- happens in some error situations).

      if not File.Is_System_File and then File.Stream /= NULL_Stream then

         --  Do not do an fclose if this is a shared file and there is at least
         --  one other instance of the stream that is open.

         if File.Shared_Status = Yes then
            declare
               P   : AFCB_Ptr;

            begin
               P := Open_Files;
               while P /= null loop
                  if P /= File and then File.Stream = P.Stream then
                     Dup_Strm := True;
                     exit;
                  end if;

                  P := P.Next;
               end loop;
            end;
         end if;

         --  Do the fclose unless this was a duplicate in the shared case

         if not Dup_Strm then
            Close_Status := fclose (File.Stream);

            if Close_Status /= 0 then
               Errno := OS_Lib.Errno;
            end if;
         end if;
      end if;

      --  Dechain file from list of open files and then free the storage

      if File.Prev = null then
         Open_Files := File.Next;
      else
         File.Prev.Next := File.Next;
      end if;

      if File.Next /= null then
         File.Next.Prev := File.Prev;
      end if;

      --  If it's a temp file, remove the corresponding record from Temp_Files,
      --  and delete the file. There are unlikely to be large numbers of temp
      --  files open, so a linear search is sufficiently efficient. Note that
      --  we don't need to check for end of list, because the file must be
      --  somewhere on the list. Note that as for Finalize, we ignore any
      --  errors while attempting the unlink operation.

      if File.Is_Temporary_File then
         declare
            Temp : access Temp_File_Record_Ptr := Temp_Files'Access;
            --  Note the double indirection here

            Discard  : int;
            New_Temp : Temp_File_Record_Ptr;

         begin
            while Temp.all.all.File /= File loop
               Temp := Temp.all.all.Next'Access;
            end loop;

            Discard  := unlink (Temp.all.all.Name'Address);
            New_Temp := Temp.all.all.Next;
            Free (Temp.all);
            Temp.all := New_Temp;
         end;
      end if;

      --  Deallocate some parts of the file structure that were kept in heap
      --  storage with the exception of system files (standard input, output
      --  and error) since they had some information allocated in the stack.

      if not File.Is_System_File then
         Free_String (File.Name);
         Free_String (File.Form);
         AFCB_Free (File);
      end if;

      File := null;

      if Close_Status /= 0 then
         Raise_Device_Error (null, Errno);
      end if;

      SSL.Unlock_Task.all;

   exception
      when others =>
         SSL.Unlock_Task.all;
         raise;
   end Close;

   ------------
   -- Delete --
   ------------

   procedure Delete (File_Ptr : access AFCB_Ptr) is
      File : AFCB_Ptr renames File_Ptr.all;

   begin
      Check_File_Open (File);

      if not File.Is_Regular_File then
         raise Use_Error with "cannot delete non-regular file";
      end if;

      declare
         Filename : aliased constant String := File.Name.all;
         Is_Temporary_File : constant Boolean := File.Is_Temporary_File;

      begin
         Close (File_Ptr);

         --  Now unlink the external file. Note that we use the full name in
         --  this unlink, because the working directory may have changed since
         --  we did the open, and we want to unlink the right file. However, if
         --  it's a temporary file, then closing it already unlinked it.

         if not Is_Temporary_File then
            if unlink (Filename'Address) = -1 then
               raise Use_Error with OS_Lib.Errno_Message;
            end if;
         end if;
      end;
   end Delete;

   -----------------
   -- End_Of_File --
   -----------------

   function End_Of_File (File : AFCB_Ptr) return Boolean is
   begin
      Check_File_Open (File);

      if feof (File.Stream) /= 0 then
         return True;

      else
         Check_Read_Status (File);

         if ungetc (fgetc (File.Stream), File.Stream) = EOF then
            clearerr (File.Stream);
            return True;
         else
            return False;
         end if;
      end if;
   end End_Of_File;

   -------------------
   -- Errno_Message --
   -------------------

   function Errno_Message
     (Name  : String;
      Errno : Integer := OS_Lib.Errno) return String
   is
   begin
      return Name & ": " & OS_Lib.Errno_Message (Err => Errno);
   end Errno_Message;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (V : in out File_IO_Clean_Up_Type) is
      pragma Warnings (Off, V);

      Fptr1   : aliased AFCB_Ptr;
      Fptr2   : AFCB_Ptr;

      Discard : int;

   begin
      --  Take a lock to protect global Open_Files data structure

      SSL.Lock_Task.all;

      --  First close all open files (the slightly complex form of this loop is
      --  required because Close nulls out its argument).

      Fptr1 := Open_Files;
      while Fptr1 /= null loop
         Fptr2 := Fptr1.Next;
         Close (Fptr1'Access);
         Fptr1 := Fptr2;
      end loop;

      --  Now unlink all temporary files. We do not bother to free the blocks
      --  because we are just about to terminate the program. We also ignore
      --  any errors while attempting these unlink operations.

      while Temp_Files /= null loop
         Discard := unlink (Temp_Files.Name'Address);
         Temp_Files := Temp_Files.Next;
      end loop;

      SSL.Unlock_Task.all;

   exception
      when others =>
         SSL.Unlock_Task.all;
         raise;
   end Finalize;

   -----------
   -- Flush --
   -----------

   procedure Flush (File : AFCB_Ptr) is
   begin
      Check_Write_Status (File);

      if fflush (File.Stream) /= 0 then
         Raise_Device_Error (File);
      end if;
   end Flush;

   ----------------
   -- Fopen_Mode --
   ----------------

   --  The fopen mode to be used is shown by the following table:

   --                                     OPEN         CREATE
   --     Append_File                     "r+"           "w+"
   --     In_File                         "r"            "w+"
   --     Out_File (Direct_IO, Stream_IO) "r+" [*]       "w"
   --     Out_File (others)               "w"            "w"
   --     Inout_File                      "r+"           "w+"

   --  [*] Except that for Out_File, if the file exists and is a fifo (i.e. a
   --  named pipe), we use "w" instead of "r+". This is necessary to make a
   --  write to the fifo block until a reader is ready.

   --  Note: we do not use "a" or "a+" for Append_File, since this would not
   --  work in the case of stream files, where even if in append file mode,
   --  you can reset to earlier points in the file. The caller must use the
   --  Append_Set routine to deal with the necessary positioning.

   --  Note: in several cases, the fopen mode used allows reading and writing,
   --  but the setting of the Ada mode is more restrictive. For instance,
   --  Create in In_File mode uses "w+" which allows writing, but the Ada mode
   --  In_File will cause any write operations to be rejected with Mode_Error
   --  in any case.

   --  Note: for the Out_File/Open cases for other than the Direct_IO case, an
   --  initial call will be made by the caller to first open the file in "r"
   --  mode to be sure that it exists. The real open, in "w" mode, will then
   --  destroy this file. This is peculiar, but that's what Ada semantics
   --  require and the ACATS tests insist on.

   --  If text file translation is required, then either "b" or "t" is appended
   --  to the mode, depending on the setting of Text.

   procedure Fopen_Mode
     (Namestr : String;
      Mode    : File_Mode;
      Text    : Boolean;
      Creat   : Boolean;
      Amethod : Character;
      Fopstr  : out Fopen_String)
   is
      Fptr : Positive;

      function is_fifo (Path : Address) return Integer;
      pragma Import (C, is_fifo, "__gnat_is_fifo");

   begin
      case Mode is
         when In_File =>
            if Creat then
               Fopstr (1) := 'w';
               Fopstr (2) := '+';
               Fptr := 3;
            else
               Fopstr (1) := 'r';
               Fptr := 2;
            end if;

         when Out_File =>
            if Amethod in 'D' | 'S'
              and then not Creat
              and then is_fifo (Namestr'Address) = 0
            then
               Fopstr (1) := 'r';
               Fopstr (2) := '+';
               Fptr := 3;
            else
               Fopstr (1) := 'w';
               Fptr := 2;
            end if;

         when Append_File
            | Inout_File
         =>
            Fopstr (1) := (if Creat then 'w' else 'r');
            Fopstr (2) := '+';
            Fptr := 3;
      end case;

      --  If text_translation_required is true then we need to append either a
      --  "t" or "b" to the string to get the right mode.

      if text_translation_required then
         Fopstr (Fptr) := (if Text then 't' else 'b');
         Fptr := Fptr + 1;
      end if;

      Fopstr (Fptr) := ASCII.NUL;
   end Fopen_Mode;

   ----------
   -- Form --
   ----------

   function Form (File : AFCB_Ptr) return String is
   begin
      if File = null then
         raise Status_Error with "Form: file not open";
      else
         return File.Form.all (1 .. File.Form'Length - 1);
      end if;
   end Form;

   ------------------
   -- Form_Boolean --
   ------------------

   function Form_Boolean
     (Form    : String;
      Keyword : String;
      Default : Boolean) return Boolean
   is
      V1, V2 : Natural;
      pragma Unreferenced (V2);

   begin
      Form_Parameter (Form, Keyword, V1, V2);

      if V1 = 0 then
         return Default;
      elsif Form (V1) = 'y' then
         return True;
      elsif Form (V1) = 'n' then
         return False;
      else
         raise Use_Error with "invalid Form";
      end if;
   end Form_Boolean;

   ------------------
   -- Form_Integer --
   ------------------

   function Form_Integer
     (Form    : String;
      Keyword : String;
      Default : Integer) return Integer
   is
      V1, V2 : Natural;
      V      : Integer;

   begin
      Form_Parameter (Form, Keyword, V1, V2);

      if V1 = 0 then
         return Default;

      else
         V := 0;

         for J in V1 .. V2 loop
            if Form (J) not in '0' .. '9' then
               raise Use_Error with "invalid Form";
            else
               V := V * 10 + Character'Pos (Form (J)) - Character'Pos ('0');
            end if;

            if V > 999_999 then
               raise Use_Error with "invalid Form";
            end if;
         end loop;

         return V;
      end if;
   end Form_Integer;

   --------------------
   -- Form_Parameter --
   --------------------

   procedure Form_Parameter
     (Form    : String;
      Keyword : String;
      Start   : out Natural;
      Stop    : out Natural)
   is
      Klen : constant Integer := Keyword'Length;

   begin
      for J in Form'First + Klen .. Form'Last - 1 loop
         if Form (J) = '='
           and then Form (J - Klen .. J - 1) = Keyword
         then
            Start := J + 1;
            Stop := Start - 1;
            while Form (Stop + 1) /= ASCII.NUL
              and then Form (Stop + 1) /= ','
            loop
               Stop := Stop + 1;
            end loop;

            return;
         end if;
      end loop;

      Start := 0;
      Stop  := 0;
   end Form_Parameter;

   -------------
   -- Is_Open --
   -------------

   function Is_Open (File : AFCB_Ptr) return Boolean is
   begin
      --  We return True if the file is open, and the underlying file stream is
      --  usable. In particular on Windows an application linked with -mwindows
      --  option set does not have a console attached. In this case standard
      --  files (Current_Output, Current_Error, Current_Input) are not created.
      --  We want Is_Open (Current_Output) to return False in this case.

      return File /= null and then fileno (File.Stream) /= -1;
   end Is_Open;

   -------------------
   -- Make_Buffered --
   -------------------

   procedure Make_Buffered
     (File    : AFCB_Ptr;
      Buf_Siz : Interfaces.C_Streams.size_t)
   is
      status : Integer;
      pragma Unreferenced (status);

   begin
      status := setvbuf (File.Stream, Null_Address, IOFBF, Buf_Siz);
   end Make_Buffered;

   ------------------------
   -- Make_Line_Buffered --
   ------------------------

   procedure Make_Line_Buffered
     (File     : AFCB_Ptr;
      Line_Siz : Interfaces.C_Streams.size_t)
   is
      status : Integer;
      pragma Unreferenced (status);

   begin
      status := setvbuf (File.Stream, Null_Address, IOLBF, Line_Siz);
      --  No error checking???
   end Make_Line_Buffered;

   ---------------------
   -- Make_Unbuffered --
   ---------------------

   procedure Make_Unbuffered (File : AFCB_Ptr) is
      status : Integer;
      pragma Unreferenced (status);

   begin
      status := setvbuf (File.Stream, Null_Address, IONBF, 0);
      --  No error checking???
   end Make_Unbuffered;

   ----------
   -- Mode --
   ----------

   function Mode (File : AFCB_Ptr) return File_Mode is
   begin
      if File = null then
         raise Status_Error with "Mode: file not open";
      else
         return File.Mode;
      end if;
   end Mode;

   ----------
   -- Name --
   ----------

   function Name (File : AFCB_Ptr) return String is
   begin
      if File = null then
         raise Status_Error with "Name: file not open";
      else
         return File.Name.all (1 .. File.Name'Length - 1);
      end if;
   end Name;

   ----------
   -- Open --
   ----------

   procedure Open
     (File_Ptr  : in out AFCB_Ptr;
      Dummy_FCB : AFCB'Class;
      Mode      : File_Mode;
      Name      : String;
      Form      : String;
      Amethod   : Character;
      Creat     : Boolean;
      Text      : Boolean;
      C_Stream  : FILEs := NULL_Stream)
   is
      pragma Warnings (Off, Dummy_FCB);
      --  Yes we know this is never assigned a value. That's intended, since
      --  all we ever use of this value is the tag for dispatching purposes.

      procedure Tmp_Name (Buffer : Address);
      pragma Import (C, Tmp_Name, "__gnat_tmp_name");
      --  Set buffer (a String address) with a temporary filename

      function Get_Case_Sensitive return Integer;
      pragma Import (C, Get_Case_Sensitive,
                     "__gnat_get_file_names_case_sensitive");

      procedure Record_AFCB;
      --  Create and record new AFCB into the runtime, note that the
      --  implementation uses the variables below which corresponds to the
      --  status of the opened file.

      File_Names_Case_Sensitive : constant Boolean := Get_Case_Sensitive /= 0;
      --  Set to indicate whether the operating system convention is for file
      --  names to be case sensitive (e.g., in Unix, set True), or not case
      --  sensitive (e.g., in Windows, set False). Declared locally to avoid
      --  breaking the Preelaborate rule that disallows function calls at the
      --  library level.

      Stream : FILEs := C_Stream;
      --  Stream which we open in response to this request

      Shared : Shared_Status_Type;
      --  Setting of Shared_Status field for file

      Fopstr : aliased Fopen_String;
      --  Mode string used in fopen call

      Formstr : aliased String (1 .. Form'Length + 1);
      --  Form string with ASCII.NUL appended, folded to lower case

      Text_Encoding : Content_Encoding;

      Tempfile : constant Boolean := Name = "";
      --  Indicates temporary file case, which is indicated by an empty file
      --  name.

      Namelen : constant Integer := max_path_len;
      --  Length required for file name, not including final ASCII.NUL.
      --  Note that we used to reference L_tmpnam here, which is not reliable
      --  since __gnat_tmp_name does not always use tmpnam.

      Namestr : aliased String (1 .. Namelen + 1);
      --  Name as given or temporary file name with ASCII.NUL appended

      Fullname : aliased String (1 .. max_path_len + 1);
      --  Full name (as required for Name function, and as stored in the
      --  control block in the Name field) with ASCII.NUL appended.

      Full_Name_Len : Integer;
      --  Length of name actually stored in Fullname

      Encoding : CRTL.Filename_Encoding;
      --  Filename encoding specified into the form parameter

      -----------------
      -- Record_AFCB --
      -----------------

      procedure Record_AFCB is
      begin
         File_Ptr := AFCB_Allocate (Dummy_FCB);

         --  Note that we cannot use an aggregate here as File_Ptr is a
         --  class-wide access to a limited type (Root_Stream_Type).

         File_Ptr.Is_Regular_File   := is_regular_file (fileno (Stream)) /= 0;
         File_Ptr.Is_System_File    := False;
         File_Ptr.Text_Encoding     := Text_Encoding;
         File_Ptr.Shared_Status     := Shared;
         File_Ptr.Access_Method     := Amethod;
         File_Ptr.Stream            := Stream;
         File_Ptr.Form              := new String'(Formstr);
         File_Ptr.Name              := new String'(Fullname
                                                     (1 .. Full_Name_Len));
         File_Ptr.Mode              := Mode;
         File_Ptr.Is_Temporary_File := Tempfile;
         File_Ptr.Encoding          := Encoding;

         Chain_File (File_Ptr);
         Append_Set (File_Ptr);
      end Record_AFCB;

   --  Start of processing for Open

   begin
      if File_Ptr /= null then
         raise Status_Error with "file already open";
      end if;

      --  Acquire form string, setting required NUL terminator

      Formstr (1 .. Form'Length) := Form;
      Formstr (Formstr'Last) := ASCII.NUL;

      --  Convert form string to lower case

      for J in Formstr'Range loop
         if Formstr (J) in 'A' .. 'Z' then
            Formstr (J) := Character'Val (Character'Pos (Formstr (J)) + 32);
         end if;
      end loop;

      --  Acquire setting of shared parameter

      declare
         V1, V2 : Natural;

      begin
         Form_Parameter (Formstr, "shared", V1, V2);

         if V1 = 0 then
            Shared := None;
         elsif Formstr (V1 .. V2) = "yes" then
            Shared := Yes;
         elsif Formstr (V1 .. V2) = "no" then
            Shared := No;
         else
            raise Use_Error with "invalid Form";
         end if;
      end;

      --  Acquire setting of encoding parameter

      declare
         V1, V2 : Natural;

      begin
         Form_Parameter (Formstr, "encoding", V1, V2);

         if V1 = 0 then
            Encoding := CRTL.Unspecified;
         elsif Formstr (V1 .. V2) = "utf8" then
            Encoding := CRTL.UTF8;
         elsif Formstr (V1 .. V2) = "8bits" then
            Encoding := CRTL.ASCII_8bits;
         else
            raise Use_Error with "invalid Form";
         end if;
      end;

      --  Acquire setting of text_translation parameter. Only needed if this is
      --  a [Wide_[Wide_]]Text_IO file, in which case we default to True, but
      --  if the Form says Text_Translation=No, we use binary mode, so new-line
      --  will be just LF, even on Windows.

      if Text then
         Text_Encoding := Default_Text;
      else
         Text_Encoding := None;
      end if;

      if Text_Encoding in Text_Content_Encoding then
         declare
            V1, V2 : Natural;

         begin
            Form_Parameter (Formstr, "text_translation", V1, V2);

            if V1 = 0 then
               null;
            elsif Formstr (V1 .. V2) = "no" then
               Text_Encoding := None;
            elsif Formstr (V1 .. V2) = "text"
              or else Formstr (V1 .. V2) = "yes"
            then
               Text_Encoding := Interfaces.C_Streams.Text;
            elsif Formstr (V1 .. V2) = "wtext" then
               Text_Encoding := Wtext;
            elsif Formstr (V1 .. V2) = "u8text" then
               Text_Encoding := U8text;
            elsif Formstr (V1 .. V2) = "u16text" then
               Text_Encoding := U16text;
            else
               raise Use_Error with "invalid Form";
            end if;
         end;
      end if;

      --  If we were given a stream (call from xxx.C_Streams.Open), then set
      --  the full name to the given one, and skip to end of processing.

      if Stream /= NULL_Stream then
         Full_Name_Len := Name'Length + 1;
         Fullname (1 .. Full_Name_Len - 1) := Name;
         Fullname (Full_Name_Len) := ASCII.NUL;

      --  Normal case of Open or Create

      else
         --  If temporary file case, get temporary file name and add to the
         --  list of temporary files to be deleted on exit.

         if Tempfile then
            if not Creat then
               raise Name_Error with "opening temp file without creating it";
            end if;

            Tmp_Name (Namestr'Address);

            if Namestr (1) = ASCII.NUL then
               raise Use_Error with "invalid temp file name";
            end if;

         --  Normal case of non-empty name given (i.e. not a temp file)

         else
            if Name'Length > Namelen then
               raise Name_Error with "file name too long";
            end if;

            Namestr (1 .. Name'Length) := Name;
            Namestr (Name'Length + 1)  := ASCII.NUL;
         end if;

         --  Get full name in accordance with the advice of RM A.8.2(22)

         full_name (Namestr'Address, Fullname'Address);

         if Fullname (1) = ASCII.NUL then
            raise Use_Error with Errno_Message (Name);
         end if;

         Full_Name_Len := 1;
         while Full_Name_Len < Fullname'Last
           and then Fullname (Full_Name_Len) /= ASCII.NUL
         loop
            Full_Name_Len := Full_Name_Len + 1;
         end loop;

         --  Fullname is generated by calling system's full_name. The problem
         --  is, full_name does nothing about the casing, so a file name
         --  comparison may generally speaking not be valid on non-case-
         --  sensitive systems, and in particular we get unexpected failures
         --  on Windows/Vista because of this. So we use s-casuti to force
         --  the name to lower case.

         if not File_Names_Case_Sensitive then
            To_Lower (Fullname (1 .. Full_Name_Len));
         end if;

         --  If Shared=None or Shared=Yes, then check for the existence of
         --  another file with exactly the same full name.

         if Shared /= No then
            declare
               P : AFCB_Ptr;

            begin
               --  Take a task lock to protect Open_Files

               SSL.Lock_Task.all;

               --  Search list of open files

               P := Open_Files;
               while P /= null loop
                  if Fullname (1 .. Full_Name_Len) = P.Name.all then

                     --  If we get a match, and either file has Shared=None,
                     --  then raise Use_Error, since we don't allow two files
                     --  of the same name to be opened unless they specify the
                     --  required sharing mode.

                     if Shared = None
                       or else P.Shared_Status = None
                     then
                        raise Use_Error with "reopening shared file";

                     --  If both files have Shared=Yes, then we acquire the
                     --  stream from the located file to use as our stream.

                     elsif Shared = Yes
                       and then P.Shared_Status = Yes
                     then
                        Stream := P.Stream;

                        Record_AFCB;
                        pragma Assert (not Tempfile);

                        exit;

                     --  Otherwise one of the files has Shared=Yes and one has
                     --  Shared=No. If the current file has Shared=No then all
                     --  is well but we don't want to share any other file's
                     --  stream. If the current file has Shared=Yes, we would
                     --  like to share a stream, but not from a file that has
                     --  Shared=No, so either way, we just continue the search.

                     else
                        null;
                     end if;
                  end if;

                  P := P.Next;
               end loop;

               SSL.Unlock_Task.all;

            exception
               when others =>
                  SSL.Unlock_Task.all;
                  raise;
            end;
         end if;

         --  Open specified file if we did not find an existing stream,
         --  otherwise we just return as there is nothing more to be done.

         if Stream /= NULL_Stream then
            return;

         else
            Fopen_Mode
              (Namestr => Namestr,
               Mode    => Mode,
               Text    => Text_Encoding in Text_Content_Encoding,
               Creat   => Creat,
               Amethod => Amethod,
               Fopstr  => Fopstr);

            --  A special case, if we are opening (OPEN case) a file and the
            --  mode returned by Fopen_Mode is not "r" or "r+", then we first
            --  make sure that the file exists as required by Ada semantics.

            if not Creat and then Fopstr (1) /= 'r' then
               if file_exists (Namestr'Address) = 0 then
                  raise Name_Error with Errno_Message (Name);
               end if;
            end if;

            --  Now open the file. Note that we use the name as given in the
            --  original Open call for this purpose, since that seems the
            --  clearest implementation of the intent. It would presumably
            --  work to use the full name here, but if there is any difference,
            --  then we should use the name used in the call.

            --  Note: for a corresponding delete, we will use the full name,
            --  since by the time of the delete, the current working directory
            --  may have changed and we do not want to delete a different file.

            Stream :=
              fopen (Namestr'Address, Fopstr'Address, Encoding);

            if Stream = NULL_Stream then

               --  Raise Name_Error if trying to open a non-existent file.
               --  Otherwise raise Use_Error.

               --  Should we raise Device_Error for ENOSPC???

               declare
                  function Is_File_Not_Found_Error
                    (Errno_Value : Integer) return Integer;
                  pragma Import
                    (C, Is_File_Not_Found_Error,
                     "__gnat_is_file_not_found_error");
                  --  Non-zero when the given errno value indicates a non-
                  --  existing file.

                  Errno   : constant Integer := OS_Lib.Errno;
                  Message : constant String := Errno_Message (Name, Errno);

               begin
                  if Is_File_Not_Found_Error (Errno) /= 0 then
                     raise Name_Error with Message;
                  else
                     raise Use_Error with Message;
                  end if;
               end;
            end if;
         end if;
      end if;

      --  Stream has been successfully located or opened, so now we are
      --  committed to completing the opening of the file. Allocate block on
      --  heap and fill in its fields.

      Record_AFCB;

      if Tempfile then
         --  Chain to temp file list, ensuring thread safety with a lock

         begin
            SSL.Lock_Task.all;
            Temp_Files :=
              new Temp_File_Record'
                (File => File_Ptr, Name => Namestr, Next => Temp_Files);
            SSL.Unlock_Task.all;

         exception
            when others =>
               SSL.Unlock_Task.all;
               raise;
         end;
      end if;
   end Open;

   ------------------------
   -- Raise_Device_Error --
   ------------------------

   procedure Raise_Device_Error
     (File  : AFCB_Ptr;
      Errno : Integer := OS_Lib.Errno)
   is
   begin
      --  Clear error status so that the same error is not reported twice

      if File /= null then
         clearerr (File.Stream);
      end if;

      raise Device_Error with OS_Lib.Errno_Message (Err => Errno);
   end Raise_Device_Error;

   --------------
   -- Read_Buf --
   --------------

   procedure Read_Buf (File : AFCB_Ptr; Buf : Address; Siz : size_t) is
      Nread : size_t;

   begin
      Nread := fread (Buf, 1, Siz, File.Stream);

      if Nread = Siz then
         return;

      elsif ferror (File.Stream) /= 0 then
         Raise_Device_Error (File);

      elsif Nread = 0 then
         raise End_Error;

      else -- 0 < Nread < Siz
         raise Data_Error with "not enough data read";
      end if;
   end Read_Buf;

   procedure Read_Buf
     (File  : AFCB_Ptr;
      Buf   : Address;
      Siz   : Interfaces.C_Streams.size_t;
      Count : out Interfaces.C_Streams.size_t)
   is
   begin
      Count := fread (Buf, 1, Siz, File.Stream);

      if Count = 0 and then ferror (File.Stream) /= 0 then
         Raise_Device_Error (File);
      end if;
   end Read_Buf;

   -----------
   -- Reset --
   -----------

   --  The reset which does not change the mode simply does a rewind

   procedure Reset (File_Ptr : access AFCB_Ptr) is
      File : AFCB_Ptr renames File_Ptr.all;
   begin
      Check_File_Open (File);
      Reset (File_Ptr, File.Mode);
   end Reset;

   --  The reset with a change in mode is done using freopen, and is not
   --  permitted except for regular files (since otherwise there is no name for
   --  the freopen, and in any case it seems meaningless).

   procedure Reset (File_Ptr : access AFCB_Ptr; Mode : File_Mode) is
      File   : AFCB_Ptr renames File_Ptr.all;
      Fopstr : aliased Fopen_String;

   begin
      Check_File_Open (File);

      --  Change of mode not allowed for shared file or file with no name or
      --  file that is not a regular file, or for a system file. Note that we
      --  allow the "change" of mode if it is not in fact doing a change.

      if Mode /= File.Mode then
         if File.Shared_Status = Yes then
            raise Use_Error with "cannot change mode of shared file";
         elsif File.Name'Length <= 1 then
            raise Use_Error with "cannot change mode of temp file";
         elsif File.Is_System_File then
            raise Use_Error with "cannot change mode of system file";
         elsif not File.Is_Regular_File then
            raise Use_Error with "cannot change mode of non-regular file";
         end if;
      end if;

      --  For In_File or Inout_File for a regular file, we can just do a rewind
      --  if the mode is unchanged, which is more efficient than doing a full
      --  reopen.

      if Mode = File.Mode
        and then Mode in Read_File_Mode
      then
         rewind (File.Stream);

      --  Here the change of mode is permitted, we do it by reopening the file
      --  in the new mode and replacing the stream with a new stream.

      else
         Fopen_Mode
           (Namestr => File.Name.all,
            Mode    => Mode,
            Text    => File.Text_Encoding in Text_Content_Encoding,
            Creat   => False,
            Amethod => File.Access_Method,
            Fopstr  => Fopstr);

         File.Stream := freopen
           (File.Name.all'Address, Fopstr'Address, File.Stream,
            File.Encoding);

         if File.Stream = NULL_Stream then
            Close (File_Ptr);
            raise Use_Error;
         else
            File.Mode := Mode;
            Append_Set (File);
         end if;
      end if;
   end Reset;

   ---------------
   -- Write_Buf --
   ---------------

   procedure Write_Buf (File : AFCB_Ptr; Buf : Address; Siz : size_t) is
   begin
      --  Note: for most purposes, the Siz and 1 parameters in the fwrite call
      --  could be reversed, but we have encountered systems where this is a
      --  better choice, since for some file formats, reversing the parameters
      --  results in records of one byte each.

      SSL.Abort_Defer.all;

      if fwrite (Buf, Siz, 1, File.Stream) /= 1 then
         if Siz /= 0 then
            SSL.Abort_Undefer.all;
            Raise_Device_Error (File);
         end if;
      end if;

      SSL.Abort_Undefer.all;
   end Write_Buf;

end System.File_IO;

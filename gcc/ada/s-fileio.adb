------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . F I L E _ I O                        --
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

with Ada.Finalization;            use Ada.Finalization;
with Ada.IO_Exceptions;           use Ada.IO_Exceptions;
with Interfaces.C_Streams;        use Interfaces.C_Streams;

with System.CRTL;
with System.Case_Util;            use System.Case_Util;
with System.Soft_Links;

with Ada.Unchecked_Deallocation;

package body System.File_IO is

   use System.File_Control_Block;

   package SSL renames System.Soft_Links;

   use type System.CRTL.size_t;

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
      Name : String (1 .. max_path_len + 1);
      Next : Temp_File_Record_Ptr;
   end record;
   --  One of these is allocated for each temporary file created

   Temp_Files : Temp_File_Record_Ptr;
   --  Points to list of names of temporary files. Note that this global
   --  variable must be properly protected to provide thread safety.

   type File_IO_Clean_Up_Type is new Controlled with null record;
   --  The closing of all open files and deletion of temporary files is an
   --  action which takes place at the end of execution of the main program.
   --  This action can be implemented using a library level object which
   --  gets finalized at the end of the main program execution. The above is
   --  a controlled type introduced for this purpose.

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

   function Get_Case_Sensitive return Integer;
   pragma Import (C, Get_Case_Sensitive,
                  "__gnat_get_file_names_case_sensitive");
   File_Names_Case_Sensitive : constant Boolean := Get_Case_Sensitive /= 0;
   --  Set to indicate whether the operating system convention is for file
   --  names to be case sensitive (e.g., in Unix, set True), or non case
   --  sensitive (e.g., in OS/2, set False).

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Free_String is new Ada.Unchecked_Deallocation (String, Pstring);

   subtype Fopen_String is String (1 .. 4);
   --  Holds open string (longest is "w+b" & nul)

   procedure Fopen_Mode
     (Mode    : File_Mode;
      Text    : Boolean;
      Creat   : Boolean;
      Amethod : Character;
      Fopstr  : out Fopen_String);
   --  Determines proper open mode for a file to be opened in the given
   --  Ada mode. Text is true for a text file and false otherwise, and
   --  Creat is true for a create call, and False for an open call. The
   --  value stored in Fopstr is a nul-terminated string suitable for a
   --  call to fopen or freopen. Amethod is the character designating
   --  the access method from the Access_Method field of the FCB.

   ----------------
   -- Append_Set --
   ----------------

   procedure Append_Set (File : AFCB_Ptr) is
   begin
      if File.Mode = Append_File then
         if fseek (File.Stream, 0, SEEK_END) /= 0 then
            raise Device_Error;
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
         raise Status_Error;
      end if;
   end Check_File_Open;

   -----------------------
   -- Check_Read_Status --
   -----------------------

   procedure Check_Read_Status (File : AFCB_Ptr) is
   begin
      if File = null then
         raise Status_Error;
      elsif File.Mode > Inout_File then
         raise Mode_Error;
      end if;
   end Check_Read_Status;

   ------------------------
   -- Check_Write_Status --
   ------------------------

   procedure Check_Write_Status (File : AFCB_Ptr) is
   begin
      if File = null then
         raise Status_Error;
      elsif File.Mode = In_File then
         raise Mode_Error;
      end if;
   end Check_Write_Status;

   -----------
   -- Close --
   -----------

   procedure Close (File : in out AFCB_Ptr) is
      Close_Status : int := 0;
      Dup_Strm     : Boolean := False;

   begin
      --  Take a task lock, to protect the global data value Open_Files

      SSL.Lock_Task.all;

      Check_File_Open (File);
      AFCB_Close (File);

      --  Sever the association between the given file and its associated
      --  external file. The given file is left closed. Do not perform system
      --  closes on the standard input, output and error files and also do
      --  not attempt to close a stream that does not exist (signalled by a
      --  null stream value -- happens in some error situations).

      if not File.Is_System_File
        and then File.Stream /= NULL_Stream
      then
         --  Do not do an fclose if this is a shared file and there is
         --  at least one other instance of the stream that is open.

         if File.Shared_Status = Yes then
            declare
               P   : AFCB_Ptr;

            begin
               P := Open_Files;
               while P /= null loop
                  if P /= File
                    and then File.Stream = P.Stream
                  then
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
         raise Device_Error;
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

   procedure Delete (File : in out AFCB_Ptr) is
   begin
      Check_File_Open (File);

      if not File.Is_Regular_File then
         raise Use_Error;
      end if;

      declare
         Filename : aliased constant String := File.Name.all;

      begin
         Close (File);

         --  Now unlink the external file. Note that we use the full name
         --  in this unlink, because the working directory may have changed
         --  since we did the open, and we want to unlink the right file!

         if unlink (Filename'Address) = -1 then
            raise Use_Error;
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

   --------------
   -- Finalize --
   --------------

   --  Note: we do not need to worry about locking against multiple task
   --  access in this routine, since it is called only from the environment
   --  task just before terminating execution.

   procedure Finalize (V : in out File_IO_Clean_Up_Type) is
      pragma Warnings (Off, V);

      Fptr1   : AFCB_Ptr;
      Fptr2   : AFCB_Ptr;

      Discard : int;
      pragma Unreferenced (Discard);

   begin
      --  Take a lock to protect global Open_Files data structure

      SSL.Lock_Task.all;

      --  First close all open files (the slightly complex form of this loop
      --  is required because Close as a side effect nulls out its argument)

      Fptr1 := Open_Files;
      while Fptr1 /= null loop
         Fptr2 := Fptr1.Next;
         Close (Fptr1);
         Fptr1 := Fptr2;
      end loop;

      --  Now unlink all temporary files. We do not bother to free the
      --  blocks because we are just about to terminate the program. We
      --  also ignore any errors while attempting these unlink operations.

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

      if fflush (File.Stream) = 0 then
         return;
      else
         raise Device_Error;
      end if;
   end Flush;

   ----------------
   -- Fopen_Mode --
   ----------------

   --  The fopen mode to be used is shown by the following table:

   --                                     OPEN         CREATE
   --     Append_File                     "r+"           "w+"
   --     In_File                         "r"            "w+"
   --     Out_File (Direct_IO)            "r+"           "w"
   --     Out_File (all others)           "w"            "w"
   --     Inout_File                      "r+"           "w+"

   --  Note: we do not use "a" or "a+" for Append_File, since this would not
   --  work in the case of stream files, where even if in append file mode,
   --  you can reset to earlier points in the file. The caller must use the
   --  Append_Set routine to deal with the necessary positioning.

   --  Note: in several cases, the fopen mode used allows reading and
   --  writing, but the setting of the Ada mode is more restrictive. For
   --  instance, Create in In_File mode uses "w+" which allows writing,
   --  but the Ada mode In_File will cause any write operations to be
   --  rejected with Mode_Error in any case.

   --  Note: for the Out_File/Open cases for other than the Direct_IO case,
   --  an initial call will be made by the caller to first open the file in
   --  "r" mode to be sure that it exists. The real open, in "w" mode, will
   --  then destroy this file. This is peculiar, but that's what Ada semantics
   --  require and the ACVT tests insist on!

   --  If text file translation is required, then either b or t is
   --  added to the mode, depending on the setting of Text.

   procedure Fopen_Mode
     (Mode    : File_Mode;
      Text    : Boolean;
      Creat   : Boolean;
      Amethod : Character;
      Fopstr  : out Fopen_String)
   is
      Fptr : Positive;

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
            if Amethod = 'D' and not Creat then
               Fopstr (1) := 'r';
               Fopstr (2) := '+';
               Fptr := 3;
            else
               Fopstr (1) := 'w';
               Fptr := 2;
            end if;

         when Inout_File | Append_File =>
            if Creat then
               Fopstr (1) := 'w';
            else
               Fopstr (1) := 'r';
            end if;

            Fopstr (2) := '+';
            Fptr := 3;

      end case;

      --  If text_translation_required is true then we need to append
      --  either a t or b to the string to get the right mode

      if text_translation_required then
         if Text then
            Fopstr (Fptr) := 't';
         else
            Fopstr (Fptr) := 'b';
         end if;

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
         raise Status_Error;
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
      Default : Boolean)
      return    Boolean
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
         raise Use_Error;
      end if;
   end Form_Boolean;

   ------------------
   -- Form_Integer --
   ------------------

   function Form_Integer
     (Form    : String;
      Keyword : String;
      Default : Integer)
      return    Integer
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
               raise Use_Error;
            else
               V := V * 10 + Character'Pos (Form (J)) - Character'Pos ('0');
            end if;

            if V > 999_999 then
               raise Use_Error;
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

   --  Start of processing for Form_Parameter

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
   end Make_Line_Buffered;

   ---------------------
   -- Make_Unbuffered --
   ---------------------

   procedure Make_Unbuffered (File : AFCB_Ptr) is
      status : Integer;
      pragma Unreferenced (status);

   begin
      status := setvbuf (File.Stream, Null_Address, IONBF, 0);
   end Make_Unbuffered;

   ----------
   -- Mode --
   ----------

   function Mode (File : AFCB_Ptr) return File_Mode is
   begin
      if File = null then
         raise Status_Error;
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
         raise Status_Error;
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
      --  set buffer (a String address) with a temporary filename

      Stream : FILEs := C_Stream;
      --  Stream which we open in response to this request

      Shared : Shared_Status_Type;
      --  Setting of Shared_Status field for file

      Fopstr : aliased Fopen_String;
      --  Mode string used in fopen call

      Formstr : aliased String (1 .. Form'Length + 1);
      --  Form string with ASCII.NUL appended, folded to lower case

      Tempfile : constant Boolean := (Name'Length = 0);
      --  Indicates temporary file case

      Namelen : constant Integer := max_path_len;
      --  Length required for file name, not including final ASCII.NUL
      --  Note that we used to reference L_tmpnam here, which is not
      --  reliable since __gnat_tmp_name does not always use tmpnam.

      Namestr : aliased String (1 .. Namelen + 1);
      --  Name as given or temporary file name with ASCII.NUL appended

      Fullname : aliased String (1 .. max_path_len + 1);
      --  Full name (as required for Name function, and as stored in the
      --  control block in the Name field) with ASCII.NUL appended.

      Full_Name_Len : Integer;
      --  Length of name actually stored in Fullname

      Encoding : System.CRTL.Filename_Encoding;
      --  Filename encoding specified into the form parameter

   begin
      if File_Ptr /= null then
         raise Status_Error;
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
            raise Use_Error;
         end if;
      end;

      --  Acquire setting of shared parameter

      declare
         V1, V2 : Natural;

      begin
         Form_Parameter (Formstr, "encoding", V1, V2);

         if V1 = 0 then
            Encoding := System.CRTL.UTF8;

         elsif Formstr (V1 .. V2) = "utf8" then
            Encoding := System.CRTL.UTF8;

         elsif Formstr (V1 .. V2) = "8bits" then
            Encoding := System.CRTL.ASCII_8bits;

         else
            raise Use_Error;
         end if;
      end;

      --  If we were given a stream (call from xxx.C_Streams.Open), then set
      --  the full name to the given one, and skip to end of processing.

      if Stream /= NULL_Stream then
         Full_Name_Len := Name'Length + 1;
         Fullname (1 .. Full_Name_Len - 1) := Name;
         Fullname (Full_Name_Len) := ASCII.Nul;

      --  Normal case of Open or Create

      else
         --  If temporary file case, get temporary file name and add
         --  to the list of temporary files to be deleted on exit.

         if Tempfile then
            if not Creat then
               raise Name_Error;
            end if;

            Tmp_Name (Namestr'Address);

            if Namestr (1) = ASCII.NUL then
               raise Use_Error;
            end if;

            --  Chain to temp file list, ensuring thread safety with a lock

            begin
               SSL.Lock_Task.all;
               Temp_Files :=
                 new Temp_File_Record'(Name => Namestr, Next => Temp_Files);
               SSL.Unlock_Task.all;

            exception
               when others =>
                  SSL.Unlock_Task.all;
                  raise;
            end;

         --  Normal case of non-null name given

         else
            if Name'Length > Namelen then
               raise Name_Error;
            end if;

            Namestr (1 .. Name'Length) := Name;
            Namestr (Name'Length + 1)  := ASCII.NUL;
         end if;

         --  Get full name in accordance with the advice of RM A.8.2(22)

         full_name (Namestr'Address, Fullname'Address);

         if Fullname (1) = ASCII.NUL then
            raise Use_Error;
         end if;

         Full_Name_Len := 1;
         while Full_Name_Len < Fullname'Last
           and then Fullname (Full_Name_Len) /= ASCII.NUL
         loop
            Full_Name_Len := Full_Name_Len + 1;
         end loop;

         --  Fullname is generated by calling system's full_name. The problem
         --  is, full_name does nothing about the casing, so a file name
         --  comparison may generally speaking not be valid on non-case
         --  sensitive systems, and in particular we get unexpected failures
         --  on Windows/Vista because of this. So we use s-casuti to force
         --  the name to lower case.

         if not File_Names_Case_Sensitive then
            To_Lower (Fullname (1 .. Full_Name_Len));
         end if;

         --  If Shared=None or Shared=Yes, then check for the existence
         --  of another file with exactly the same full name.

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
                        raise Use_Error;

                     --  If both files have Shared=Yes, then we acquire the
                     --  stream from the located file to use as our stream.

                     elsif Shared = Yes
                       and then P.Shared_Status = Yes
                     then
                        Stream := P.Stream;
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

         --  Open specified file if we did not find an existing stream

         if Stream = NULL_Stream then
            Fopen_Mode (Mode, Text, Creat, Amethod, Fopstr);

            --  A special case, if we are opening (OPEN case) a file and the
            --  mode returned by Fopen_Mode is not "r" or "r+", then we first
            --  make sure that the file exists as required by Ada semantics.

            if Creat = False and then Fopstr (1) /= 'r' then
               if file_exists (Namestr'Address) = 0 then
                  raise Name_Error;
               end if;
            end if;

            --  Now open the file. Note that we use the name as given in the
            --  original Open call for this purpose, since that seems the
            --  clearest implementation of the intent. It would presumably
            --  work to use the full name here, but if there is any difference,
            --  then we should use the name used in the call.

            --  Note: for a corresponding delete, we will use the full name,
            --  since by the time of the delete, the current working directory
            --  may have changed and we do not want to delete a different file!

            Stream := fopen (Namestr'Address, Fopstr'Address, Encoding);

            if Stream = NULL_Stream then
               if file_exists (Namestr'Address) = 0 then
                  raise Name_Error;
               else
                  raise Use_Error;
               end if;
            end if;
         end if;
      end if;

      --  Stream has been successfully located or opened, so now we are
      --  committed to completing the opening of the file. Allocate block
      --  on heap and fill in its fields.

      File_Ptr := AFCB_Allocate (Dummy_FCB);

      File_Ptr.Is_Regular_File   := (is_regular_file (fileno (Stream)) /= 0);
      File_Ptr.Is_System_File    := False;
      File_Ptr.Is_Text_File      := Text;
      File_Ptr.Shared_Status     := Shared;
      File_Ptr.Access_Method     := Amethod;
      File_Ptr.Stream            := Stream;
      File_Ptr.Form              := new String'(Formstr);
      File_Ptr.Name              := new String'(Fullname (1 .. Full_Name_Len));
      File_Ptr.Mode              := Mode;
      File_Ptr.Is_Temporary_File := Tempfile;
      File_Ptr.Encoding          := Encoding;

      Chain_File (File_Ptr);
      Append_Set (File_Ptr);
   end Open;

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
         raise Device_Error;

      elsif Nread = 0 then
         raise End_Error;

      else -- 0 < Nread < Siz
         raise Data_Error;
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
         raise Device_Error;
      end if;
   end Read_Buf;

   -----------
   -- Reset --
   -----------

   --  The reset which does not change the mode simply does a rewind

   procedure Reset (File : in out AFCB_Ptr) is
   begin
      Check_File_Open (File);
      Reset (File, File.Mode);
   end Reset;

   --  The reset with a change in mode is done using freopen, and is
   --  not permitted except for regular files (since otherwise there
   --  is no name for the freopen, and in any case it seems meaningless)

   procedure Reset (File : in out AFCB_Ptr; Mode : File_Mode) is
      Fopstr : aliased Fopen_String;

   begin
      Check_File_Open (File);

      --  Change of mode not allowed for shared file or file with no name
      --  or file that is not a regular file, or for a system file.

      if File.Shared_Status = Yes
        or else File.Name'Length <= 1
        or else File.Is_System_File
        or else not File.Is_Regular_File
      then
         raise Use_Error;

      --  For In_File or Inout_File for a regular file, we can just do a
      --  rewind if the mode is unchanged, which is more efficient than
      --  doing a full reopen.

      elsif Mode = File.Mode
        and then Mode <= Inout_File
      then
         rewind (File.Stream);

      --  Here the change of mode is permitted, we do it by reopening the
      --  file in the new mode and replacing the stream with a new stream.

      else
         Fopen_Mode
           (Mode, File.Is_Text_File, False, File.Access_Method, Fopstr);

         File.Stream := freopen
           (File.Name.all'Address, Fopstr'Address, File.Stream, File.Encoding);

         if File.Stream = NULL_Stream then
            Close (File);
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
      --  Note: for most purposes, the Siz and 1 parameters in the fwrite
      --  call could be reversed, but on VMS, this is a better choice, since
      --  for some file formats, reversing the parameters results in records
      --  of one byte each.

      SSL.Abort_Defer.all;

      if fwrite (Buf, Siz, 1, File.Stream) /= 1 then
         if Siz /= 0 then
            SSL.Abort_Undefer.all;
            raise Device_Error;
         end if;
      end if;

      SSL.Abort_Undefer.all;
   end Write_Buf;

end System.File_IO;

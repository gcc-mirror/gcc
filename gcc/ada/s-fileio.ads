------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . F I L E _ I O                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.1 $
--                                                                          --
--          Copyright (C) 1992-2001, Free Software Foundation, Inc.         --
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

--  This package provides support for the routines described in (RM A.8.2)
--  which are common to Text_IO, Direct_IO, Sequential_IO and Stream_IO.

with Interfaces.C_Streams;

with System.File_Control_Block;

package System.File_IO is

   package FCB renames System.File_Control_Block;
   package ICS renames Interfaces.C_Streams;

   ---------------------
   -- File Management --
   ---------------------

   procedure Open
     (File_Ptr  : in out FCB.AFCB_Ptr;
      Dummy_FCB : in out FCB.AFCB'Class;
      Mode      : FCB.File_Mode;
      Name      : String;
      Form      : String;
      Amethod   : Character;
      Creat     : Boolean;
      Text      : Boolean;
      C_Stream  : ICS.FILEs := ICS.NULL_Stream);
   --  This routine is used for both Open and Create calls:
   --
   --    File_Ptr is the file type, which must be null on entry
   --    (i.e. the file must be closed before the call).
   --
   --    Dummy_FCB is a default initialized file control block of appropriate
   --    type. Note that the tag of this record indicates the type and length
   --    of the control block. This control block is used only for the purpose
   --    of providing the controlling argument for calling the write version
   --    of Allocate_AFCB. It has no other purpose, and its fields are never
   --    read or written.
   --
   --    Mode is the required mode
   --
   --    Name is the file name, with a null string indicating that a temporary
   --    file is to be created (only permitted in create mode, not open mode)
   --
   --    Creat is True for a create call, and false for an open call
   --
   --    Text is set True to open the file in text mode (w+t or r+t) instead
   --    of the usual binary mode open (w+b or r+b).
   --
   --    Form is the form string given in the open or create call, this is
   --    stored in the AFCB, but otherwise is not used by this or any other
   --    routine in this unit (except Form which retrieves the original value)
   --
   --    Amethod indicates the access method
   --
   --      D = Direct_IO
   --      Q = Sequential_IO
   --      S = Stream_IO
   --      T = Text_IO
   --      W = Wide_Text_IO
   --
   --    C_Stream is left at its default value for the normal case of an
   --    Open or Create call as defined in the RM. The only time this is
   --    non-null is for the Open call from Ada.xxx_IO.C_Streams.Open.
   --
   --  On return, if the open/create succeeds, then the fields of File are
   --  filled in, and this value is copied to the heap. File_Ptr points to
   --  this allocated file control block. If the open/create fails, then the
   --  fields of File are undefined, and File_Ptr is unchanged.

   procedure Close (File : in out FCB.AFCB_Ptr);
   --  The file is closed, all storage associated with it is released, and
   --  File is set to null. Note that this routine calls AFCB_Close to perform
   --  any specialized close actions, then closes the file at the system level,
   --  then frees the mode and form strings, and finally calls AFCB_Free to
   --  free the file control block itself, setting File to null.

   procedure Delete (File : in out FCB.AFCB_Ptr);
   --  The indicated file is unlinked

   procedure Reset (File : in out FCB.AFCB_Ptr; Mode : in FCB.File_Mode);
   --  The file is reset, and the mode changed as indicated.

   procedure Reset (File : in out FCB.AFCB_Ptr);
   --  The files is reset, and the mode is unchanged

   function Mode (File : in FCB.AFCB_Ptr) return FCB.File_Mode;
   --  Returns the mode as supplied by create, open or reset

   function Name (File : in FCB.AFCB_Ptr) return String;
   --  Returns the file name as supplied by Open or Create. Raises Use_Error
   --  if used with temporary files or standard files.

   function Form (File : in FCB.AFCB_Ptr) return String;
   --  Returns the form as supplied by create, open or reset
   --  The string is normalized to all lower case letters.

   function Is_Open (File : in FCB.AFCB_Ptr) return Boolean;
   --  Determines if file is open or not

   ----------------------
   -- Utility Routines --
   ----------------------

   --  Some internal routines not defined in A.8.2. These are routines which
   --  provide required common functionality shared by separate packages.

   procedure Chain_File (File : FCB.AFCB_Ptr);
   --  Used to chain the given file into the list of open files. Normally this
   --  is done implicitly by Open. Chain_File is used for the spcial cases of
   --  the system files defined by Text_IO (stdin, stdout, stderr) which are
   --  not opened in the normal manner. Note that the caller is responsible
   --  for task lock out to protect the global data structures if this is
   --  necessary (it is needed for the calls from within this unit itself,
   --  but not required for the calls from Text_IO and Wide_Text_IO that
   --  are made during elaboration of the environment task).

   procedure Check_File_Open (File : FCB.AFCB_Ptr);
   --  If the current file is not open, then Status_Error is raised.
   --  Otherwise control returns normally (with File pointing to the
   --  control block for the open file.

   procedure Check_Read_Status (File : FCB.AFCB_Ptr);
   --  If the current file is not open, then Status_Error is raised. If
   --  the file is open, then the mode is checked to ensure that reading
   --  is permitted, and if not Mode_Error is raised, otherwise control
   --  returns normally.

   procedure Check_Write_Status (File : FCB.AFCB_Ptr);
   --  If the current file is not open, then Status_Error is raised. If
   --  the file is open, then the mode is checked to ensure that writing
   --  is permitted, and if not Mode_Error is raised, otherwise control
   --  returns normally.

   function End_Of_File (File : FCB.AFCB_Ptr) return Boolean;
   --  File must be opened in read mode. True is returned if the stream is
   --  currently positioned at the end of file, otherwise False is returned.
   --  The position of the stream is not affected.

   procedure Flush (File : FCB.AFCB_Ptr);
   --  Flushes the stream associated with the given file. The file must be
   --  open and in write mode (if not, an appropriate exception is raised)

   function Form_Boolean
     (Form    : String;
      Keyword : String;
      Default : Boolean)
      return    Boolean;
   --  Searches form string for an entry of the form Keyword=xx where xx is
   --  either Yes/No or y/n. Returns True if Yes or Y is found, False if No
   --  or N is found. If the keyword parameter is not found, returns the
   --  value given as Default. May raise Use_Error if a form string syntax
   --  error is detected. Keyword and Form must be in lower case.

   function Form_Integer
     (Form    : String;
      Keyword : String;
      Default : Integer)
      return    Integer;
   --  Searches form string for an entry of the form Keyword=xx where xx is
   --  an unsigned decimal integer in the range 0 to 999_999. Returns this
   --  integer value if it is found. If the keyword parameter is not found,
   --  returns the value given as Default. Raise Use_Error if a form string
   --  syntax error is detected. Keyword and Form must be in lower case.

   procedure Form_Parameter
     (Form    : String;
      Keyword : String;
      Start   : out Natural;
      Stop    : out Natural);
   --  Searches form string for an entry of the form Keyword=xx and if found
   --  Sets Start and Stop to the first and last characters of xx. Keyword
   --  and Form must be in lower case. If no entry matches, then Start and
   --  Stop are set to zero on return. Use_Error is raised if a malformed
   --  string is detected, but there is no guarantee of full syntax checking.

   procedure Read_Buf
     (File : FCB.AFCB_Ptr;
      Buf  : Address;
      Siz  : Interfaces.C_Streams.size_t);
   --  Reads Siz bytes from File.Stream into Buf. The caller has checked
   --  that the file is open in read mode. Raises an exception if Siz bytes
   --  cannot be read (End_Error if no data was read, Data_Error if a partial
   --  buffer was read, Device_Error if an error occurs).

   procedure Read_Buf
     (File  : FCB.AFCB_Ptr;
      Buf   : Address;
      Siz   : in Interfaces.C_Streams.size_t;
      Count : out Interfaces.C_Streams.size_t);
   --  Reads Siz bytes from File.Stream into Buf. The caller has checked
   --  that the file is open in read mode. Device Error is raised if an error
   --  occurs. Count is the actual number of bytes read, which may be less
   --  than Siz if the end of file is encountered.

   procedure Append_Set (File : FCB.AFCB_Ptr);
   --  If the mode of the file is Append_File, then the file is positioned
   --  at the end of file using fseek, otherwise this call has no effect.

   procedure Write_Buf
     (File : FCB.AFCB_Ptr;
      Buf  : Address;
      Siz  : Interfaces.C_Streams.size_t);
   --  Writes size_t bytes to File.Stream from Buf. The caller has checked
   --  that the file is open in write mode. Raises Device_Error if the
   --  complete buffer cannot be written.

   procedure Make_Unbuffered (File : FCB.AFCB_Ptr);

   procedure Make_Line_Buffered
     (File     : FCB.AFCB_Ptr;
      Line_Siz : Interfaces.C_Streams.size_t);

   procedure Make_Buffered
     (File     : FCB.AFCB_Ptr;
      Buf_Siz  : Interfaces.C_Streams.size_t);

private
   pragma Inline (Check_Read_Status);
   pragma Inline (Check_Write_Status);
   pragma Inline (Form);
   pragma Inline (Mode);

end System.File_IO;

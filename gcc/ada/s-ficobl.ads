------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--              S Y S T E M . F I L E _ C O N T R O L _ B L O C K           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 1992-2000 Free Software Foundation, Inc.        --
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

--  This package contains the declaration of the basic file control block
--  shared between Text_IO, Sequential_IO, Direct_IO and Streams.Stream_IO.
--  The actual control blocks are derived from this block by extension. The
--  control block is itself derived from Ada.Streams.Root_Stream_Type which
--  facilitates implementation of Stream_IO.Stream and Text_Streams.Stream.

with Ada.Streams;
with Interfaces.C_Streams;

package System.File_Control_Block is

   -----------------------------
   --  Ada File Control Block --
   -----------------------------

   --  The Ada file control block is an abstract extension of the root
   --  stream type. This allows a file to be treated directly as a stream
   --  for the purposes of Stream_IO, or stream operations on a text file.
   --  The individual I/O packages extend this type with package specific
   --  fields to create the concrete types to which the routines in this
   --  package can be applied.

   --  The type File_Type in the individual packages is an access to the
   --  extended file control block. The value is null if the file is not
   --  open, and a pointer to the control block if the file is open.

   type Pstring is access all String;
   --  Used to hold name and form strings

   type File_Mode is (In_File, Inout_File, Out_File, Append_File);
   --  File mode (union of file modes permitted by individual packages,
   --  the types File_Mode in the individual packages are declared to
   --  allow easy conversion to and from this general type.

   type Shared_Status_Type is (Yes, No, None);
   --  This type is used to define the sharing status of a file. The default
   --  setting of None is used if no "shared=xxx" appears in the form string
   --  when a file is created or opened. For a file with Shared_Status set to
   --  None, Use_Error will be raised if any other file is opened or created
   --  with the same full name. Yes/No are set in response to the presence
   --  of "shared=yes" or "shared=no" in the form string. In either case it
   --  is permissible to have multiple files opened with the same full name.
   --  All files opened simultaneously with "shared=yes" will share the same
   --  stream with the semantics specified in the RM for file sharing. All
   --  files opened with "shared=no" will have their own stream.

   type AFCB;
   type AFCB_Ptr is access all AFCB'Class;

   type AFCB is abstract new Ada.Streams.Root_Stream_Type with record

      Stream : Interfaces.C_Streams.FILEs;
      --  The file descriptor

      Name : Pstring;
      --  A pointer to the file name. The file name is null for temporary
      --  files, and also for standard files (stdin, stdout, stderr). The
      --  name is always null-terminated if it is non-null.

      Form : Pstring;
      --  A pointer to the form string. This is the string used in the
      --  fopen call, and must be supplied by the caller (there are no
      --  defaults at this level). The string is always null-terminated.

      Mode : File_Mode;
      --  The file mode. No checks are made that the mode is consistent
      --  with the form used to fopen the file.

      Is_Regular_File : Boolean;
      --  A flag indicating if the file is a regular file

      Is_Temporary_File : Boolean;
      --  A flag set only for temporary files (i.e. files created using the
      --  Create function with a null name parameter, using tmpfile). This
      --  is currently not used since temporary files are deleted by the
      --  operating system, but it is set properly in case some systems
      --  need this information in the future.

      Is_System_File : Boolean;
      --  A flag set only for system files (stdin, stdout, stderr)

      Is_Text_File : Boolean;
      --  A flag set if the file was opened in text mode

      Shared_Status : Shared_Status_Type;
      --  Indicates sharing status of file, see description of type above

      Access_Method : Character;
      --  Set to 'Q', 'S', 'T, 'D' for Sequential_IO, Stream_IO, Text_IO
      --  Direct_IO file (used to validate file sharing request).

      Next : AFCB_Ptr;
      Prev : AFCB_Ptr;
      --  All open files are kept on a doubly linked chain, with these
      --  pointers used to maintain the next and previous pointers.

   end record;

   ----------------------------------
   -- Primitive Operations of AFCB --
   ----------------------------------

   --  Note that we inherit the abstract operations Read and Write from
   --  the base type. These must be overridden by the individual file
   --  access methods to provide Stream Read/Write access.

   function AFCB_Allocate (Control_Block : AFCB) return AFCB_Ptr is abstract;
   --  Given a control block, allocate space for a control block of the same
   --  type on the heap, and return the pointer to this allocated block. Note
   --  that the argument Control_Block is not used other than as the argument
   --  that controls which version of AFCB_Allocate is called.

   procedure AFCB_Close (File : access AFCB) is abstract;
   --  Performs any specialized close actions on a file before the file is
   --  actually closed at the system level. This is called by Close, and
   --  the reason we need the primitive operation is for the automatic
   --  close operations done as part of finalization.

   procedure AFCB_Free (File : access AFCB) is abstract;
   --  Frees the AFCB referenced by the given parameter. It is not necessary
   --  to free the strings referenced by the Form and Name fields, but if the
   --  extension has any other heap objects, they must be freed as well. This
   --  procedure must be overridden by each individual file package.

end System.File_Control_Block;

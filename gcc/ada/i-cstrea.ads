------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                 I N T E R F A C E S . C _ S T R E A M S                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1995-2010, Free Software Foundation, Inc.         --
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

--  This package is a thin binding to selected functions in the C
--  library that provide a complete interface for handling C streams.

with System.CRTL;

package Interfaces.C_Streams is
   pragma Preelaborate;

   subtype chars is System.CRTL.chars;
   subtype FILEs is System.CRTL.FILEs;
   subtype int is System.CRTL.int;
   subtype long is System.CRTL.long;
   subtype size_t is System.CRTL.size_t;
   subtype voids is System.Address;

   NULL_Stream : constant FILEs;
   --  Value returned (NULL in C) to indicate an fdopen/fopen/tmpfile error

   ----------------------------------
   -- Constants Defined in stdio.h --
   ----------------------------------

   EOF : constant int;
   --  Used by a number of routines to indicate error or end of file

   IOFBF : constant int;
   IOLBF : constant int;
   IONBF : constant int;
   --  Used to indicate buffering mode for setvbuf call

   L_tmpnam : constant int;
   --  Maximum length of file name that can be returned by tmpnam

   SEEK_CUR : constant int;
   SEEK_END : constant int;
   SEEK_SET : constant int;
   --  Used to indicate origin for fseek call

   function stdin  return FILEs;
   function stdout return FILEs;
   function stderr return FILEs;
   --  Streams associated with standard files

   --------------------------
   -- Standard C functions --
   --------------------------

   --  The functions selected below are ones that are available in
   --  UNIX (but not necessarily in ANSI C). These are very thin
   --  interfaces which copy exactly the C headers. For more
   --  documentation on these functions, see the Microsoft C "Run-Time
   --  Library Reference" (Microsoft Press, 1990, ISBN 1-55615-225-6),
   --  which includes useful information on system compatibility.

   procedure clearerr (stream : FILEs) renames System.CRTL.clearerr;

   function fclose (stream : FILEs) return int renames System.CRTL.fclose;

   function fdopen (handle : int; mode : chars) return FILEs
     renames System.CRTL.fdopen;

   function feof (stream : FILEs) return int;

   function ferror (stream : FILEs) return int;

   function fflush (stream : FILEs) return int renames System.CRTL.fflush;

   function fgetc (stream : FILEs) return int renames System.CRTL.fgetc;

   function fgets (strng : chars; n : int; stream : FILEs) return chars
     renames System.CRTL.fgets;

   function fileno (stream : FILEs) return int;

   function fopen
     (filename : chars;
      mode     : chars;
      encoding : System.CRTL.Filename_Encoding := System.CRTL.UTF8)
      return FILEs
     renames System.CRTL.fopen;
   --  Note: to maintain target independence, use text_translation_required,
   --  a boolean variable defined in sysdep.c to deal with the target
   --  dependent text translation requirement. If this variable is set,
   --  then b/t should be appended to the standard mode argument to set
   --  the text translation mode off or on as required.

   function fputc (C : int; stream : FILEs) return int
     renames System.CRTL.fputc;

   function fputs (Strng : chars; Stream : FILEs) return int
     renames System.CRTL.fputs;

   function fread
     (buffer : voids;
      size   : size_t;
      count  : size_t;
      stream : FILEs) return size_t;

   function fread
     (buffer : voids;
      index  : size_t;
      size   : size_t;
      count  : size_t;
      stream : FILEs) return size_t;
   --  Same as normal fread, but has a parameter 'index' that indicates
   --  the starting index for the read within 'buffer' (which must be the
   --  address of the beginning of a whole array object with an assumed
   --  zero base). This is needed for systems that do not support taking
   --  the address of an element within an array.

   function freopen
     (filename : chars;
      mode     : chars;
      stream   : FILEs;
      encoding : System.CRTL.Filename_Encoding := System.CRTL.UTF8)
      return FILEs
     renames System.CRTL.freopen;

   function fseek
     (stream : FILEs;
      offset : long;
      origin : int) return int
     renames System.CRTL.fseek;

   function ftell (stream : FILEs) return long
     renames System.CRTL.ftell;

   function fwrite
     (buffer : voids;
      size   : size_t;
      count  : size_t;
      stream : FILEs) return size_t;

   function isatty (handle : int) return int renames System.CRTL.isatty;

   procedure mktemp (template : chars) renames System.CRTL.mktemp;
   --  The return value (which is just a pointer to template) is discarded

   procedure rewind (stream : FILEs) renames System.CRTL.rewind;

   function setvbuf
     (stream : FILEs;
      buffer : chars;
      mode   : int;
      size   : size_t) return int;

   procedure tmpnam (string : chars) renames System.CRTL.tmpnam;
   --  The parameter must be a pointer to a string buffer of at least L_tmpnam
   --  bytes (the call with a null parameter is not supported). The returned
   --  value, which is just a copy of the input argument, is discarded.

   function tmpfile return FILEs renames System.CRTL.tmpfile;

   function ungetc (c : int; stream : FILEs) return int
     renames System.CRTL.ungetc;

   function unlink (filename : chars) return int
     renames System.CRTL.unlink;

   ---------------------
   -- Extra functions --
   ---------------------

   --  These functions supply slightly thicker bindings than those above.
   --  They are derived from functions in the C Run-Time Library, but may
   --  do a bit more work than just directly calling one of the Library
   --  functions.

   function file_exists (name : chars) return int;
   --  Tests if given name corresponds to an existing file

   function is_regular_file (handle : int) return int;
   --  Tests if given handle is for a regular file (result 1) or for a
   --  non-regular file (pipe or device, result 0).

   ---------------------------------
   -- Control of Text/Binary Mode --
   ---------------------------------

   --  If text_translation_required is true, then the following functions may
   --  be used to dynamically switch a file from binary to text mode or vice
   --  versa. These functions have no effect if text_translation_required is
   --  false (i.e. in normal unix mode). Use fileno to get a stream handle.

   procedure set_binary_mode (handle : int);
   procedure set_text_mode   (handle : int);

   ----------------------------
   -- Full Path Name support --
   ----------------------------

   procedure full_name (nam : chars; buffer : chars);
   --  Given a NUL terminated string representing a file name, returns in
   --  buffer a NUL terminated string representing the full path name for
   --  the file name. On systems where it is relevant the drive is also part
   --  of the full path name. It is the responsibility of the caller to
   --  pass an actual parameter for buffer that is big enough for any full
   --  path name. Use max_path_len given below as the size of buffer.

   max_path_len : Integer;
   --  Maximum length of an allowable full path name on the system,
   --  including a terminating NUL character.

private
   --  The following functions are specialized in the body depending on the
   --  operating system.

   pragma Inline (fread);
   pragma Inline (fwrite);
   pragma Inline (setvbuf);

   pragma Import (C, file_exists, "__gnat_file_exists");
   pragma Import (C, is_regular_file, "__gnat_is_regular_file_fd");

   pragma Import (C, set_binary_mode, "__gnat_set_binary_mode");
   pragma Import (C, set_text_mode, "__gnat_set_text_mode");

   pragma Import (C, max_path_len, "__gnat_max_path_len");
   pragma Import (C, full_name, "__gnat_full_name");

   --  The following may be implemented as macros, and so are supported
   --  via an interface function in the a-cstrea.c file.

   pragma Import (C, feof,   "__gnat_feof");
   pragma Import (C, ferror, "__gnat_ferror");
   pragma Import (C, fileno, "__gnat_fileno");

   pragma Import (C, EOF, "__gnat_constant_eof");
   pragma Import (C, IOFBF, "__gnat_constant_iofbf");
   pragma Import (C, IOLBF, "__gnat_constant_iolbf");
   pragma Import (C, IONBF, "__gnat_constant_ionbf");
   pragma Import (C, SEEK_CUR, "__gnat_constant_seek_cur");
   pragma Import (C, SEEK_END, "__gnat_constant_seek_end");
   pragma Import (C, SEEK_SET, "__gnat_constant_seek_set");
   pragma Import (C, L_tmpnam, "__gnat_constant_l_tmpnam");

   pragma Import (C, stderr, "__gnat_constant_stderr");
   pragma Import (C, stdin,  "__gnat_constant_stdin");
   pragma Import (C, stdout, "__gnat_constant_stdout");

   NULL_Stream : constant FILEs := System.Null_Address;

end Interfaces.C_Streams;

------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                 I N T E R F A C E S . C _ S T R E A M S                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--          Copyright (C) 1995-2002 Free Software Foundation, Inc.          --
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

--  This package is a thin binding to selected functions in the C
--  library that provide a complete interface for handling C streams.

with System.Parameters;

package Interfaces.C_Streams is
   pragma Preelaborate;

   --  Note: the reason we do not use the types that are in Interfaces.C is
   --  that we want to avoid dragging in the code in this unit if possible.

   subtype chars is System.Address;
   --  Pointer to null-terminated array of characters

   subtype FILEs is System.Address;
   --  Corresponds to the C type FILE*

   subtype voids is System.Address;
   --  Corresponds to the C type void*

   subtype int is Integer;
   --  Note: the above type is a subtype deliberately, and it is part of
   --  this spec that the above correspondence is guaranteed. This means
   --  that it is legitimate to, for example, use Integer instead of int.
   --  We provide this synonym for clarity, but in some cases it may be
   --  convenient to use the underlying types (for example to avoid an
   --  unnecessary dependency of a spec on the spec of this unit).

   type long is range -(2 ** (System.Parameters.long_bits - 1))
      .. +(2 ** (System.Parameters.long_bits - 1)) - 1;
   --  Note: the above type also used to be a subtype, but the correspondence
   --  was unused so it was made into a parameterized type to avoid having
   --  multiple versions of this spec for systems where long /= Long_Integer.

   type size_t is mod 2 ** Standard'Address_Size;

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

   --  The functions selected below are ones that are available in DOS,
   --  OS/2, UNIX and Xenix (but not necessarily in ANSI C). These are
   --  very thin interfaces which copy exactly the C headers. For more
   --  documentation on these functions, see the Microsoft C "Run-Time
   --  Library Reference" (Microsoft Press, 1990, ISBN 1-55615-225-6),
   --  which includes useful information on system compatibility.

   procedure clearerr (stream : FILEs);

   function fclose (stream : FILEs) return int;

   function fdopen (handle : int; mode : chars) return FILEs;

   function feof (stream : FILEs) return int;

   function ferror (stream : FILEs) return int;

   function fflush (stream : FILEs) return int;

   function fgetc (stream : FILEs) return int;

   function fgets (strng : chars; n : int; stream : FILEs) return chars;

   function fileno (stream : FILEs) return int;

   function fopen (filename : chars; Mode : chars) return FILEs;
   --  Note: to maintain target independence, use text_translation_required,
   --  a boolean variable defined in a-sysdep.c to deal with the target
   --  dependent text translation requirement. If this variable is set,
   --  then b/t should be appended to the standard mode argument to set
   --  the text translation mode off or on as required.

   function fputc (C : int; stream : FILEs) return int;

   function fputs (Strng : chars; Stream : FILEs) return int;

   function fread
     (buffer : voids;
      size   : size_t;
      count  : size_t;
      stream : FILEs)
      return   size_t;

   function fread
     (buffer : voids;
      index  : size_t;
      size   : size_t;
      count  : size_t;
      stream : FILEs)
      return   size_t;
   --  Same as normal fread, but has a parameter 'index' that indicates
   --  the starting index for the read within 'buffer' (which must be the
   --  address of the beginning of a whole array object with an assumed
   --  zero base). This is needed for systems that do not support taking
   --  the address of an element within an array.

   function freopen
     (filename : chars;
      mode     : chars;
      stream   : FILEs)
      return     FILEs;

   function fseek
     (stream : FILEs;
      offset : long;
      origin : int)
      return   int;

   function ftell (stream : FILEs) return long;

   function fwrite
     (buffer : voids;
      size   : size_t;
      count  : size_t;
      stream : FILEs)
      return   size_t;

   function isatty (handle : int) return int;

   procedure mktemp (template : chars);
   --  The return value (which is just a pointer to template) is discarded

   procedure rewind (stream : FILEs);

   function setvbuf
     (stream : FILEs;
      buffer : chars;
      mode   : int;
      size   : size_t)
      return   int;

   procedure tmpnam (string : chars);
   --  The parameter must be a pointer to a string buffer of at least L_tmpnam
   --  bytes (the call with a null parameter is not supported). The returned
   --  value, which is just a copy of the input argument, is discarded.

   function tmpfile return FILEs;

   function ungetc (c : int; stream : FILEs) return int;

   function unlink (filename : chars) return int;

   ---------------------
   -- Extra functions --
   ---------------------

   --  These functions supply slightly thicker bindings than those above.
   --  They are derived from functions in the C Run-Time Library, but may
   --  do a bit more work than just directly calling one of the Library
   --  functions.

   function file_exists (name : chars) return int;
   --  Tests if given name corresponds to an existing file.

   function is_regular_file (handle : int) return int;
   --  Tests if given handle is for a regular file (result 1) or for
   --  a non-regular file (pipe or device, result 0).

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

   --  The following routines are always functions in C, and thus can be
   --  imported directly into Ada without any intermediate C needed

   pragma Import (C, clearerr);
   pragma Import (C, fclose);
   pragma Import (C, fdopen);
   pragma Import (C, fflush);
   pragma Import (C, fgetc);
   pragma Import (C, fgets);
   pragma Import (C, fopen);
   pragma Import (C, fputc);
   pragma Import (C, fputs);
   pragma Import (C, freopen);
   pragma Import (C, fseek);
   pragma Import (C, ftell);
   pragma Import (C, isatty);
   pragma Import (C, mktemp);
   pragma Import (C, rewind);
   pragma Import (C, tmpnam);
   pragma Import (C, tmpfile);
   pragma Import (C, ungetc);
   pragma Import (C, unlink);

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

------------------------------------------------------------------------------
--                                                                          --
--                        GNAT RUN-TIME COMPONENTS                          --
--                                                                          --
--                          S Y S T E M . C R T L                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2003-2017, Free Software Foundation, Inc.         --
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

--  This package provides the low level interface to the C runtime library

pragma Compiler_Unit_Warning;

with System.Parameters;

package System.CRTL is
   pragma Preelaborate;

   subtype chars is System.Address;
   --  Pointer to null-terminated array of characters
   --  Should use Interfaces.C.Strings types instead, but this causes bootstrap
   --  issues as i-c contains Ada 2005 specific features, not compatible with
   --  older, Ada 95-only base compilers???

   subtype DIRs is System.Address;
   --  Corresponds to the C type DIR*

   subtype FILEs is System.Address;
   --  Corresponds to the C type FILE*

   subtype int is Integer;

   type long is range -(2 ** (System.Parameters.long_bits - 1))
                   .. +(2 ** (System.Parameters.long_bits - 1)) - 1;

   subtype off_t is Long_Integer;

   type size_t is mod 2 ** Standard'Address_Size;

   type ssize_t is range -(2 ** (Standard'Address_Size - 1))
                      .. +(2 ** (Standard'Address_Size - 1)) - 1;

   type int64 is new Long_Long_Integer;
   --  Note: we use Long_Long_Integer'First instead of -2 ** 63 to allow this
   --  unit to compile when using custom target configuration files where the
   --  maximum integer is 32 bits. This is useful for static analysis tools
   --  such as SPARK or CodePeer. In the normal case, Long_Long_Integer is
   --  always 64-bits so there is no difference.

   type Filename_Encoding is (UTF8, ASCII_8bits, Unspecified);
   for Filename_Encoding use (UTF8 => 0, ASCII_8bits => 1, Unspecified => 2);
   pragma Convention (C, Filename_Encoding);
   --  Describes the filename's encoding

   --------------------
   -- GCC intrinsics --
   --------------------

   --  The following functions are imported with convention Intrinsic so that
   --  we take advantage of back-end builtins if present (else we fall back
   --  to C library functions by the same names).

   function strlen (A : System.Address) return size_t;
   pragma Import (Intrinsic, strlen, "strlen");

   procedure strncpy (dest, src : System.Address; n : size_t);
   pragma Import (Intrinsic, strncpy, "strncpy");

   -------------------------------
   -- Other C runtime functions --
   -------------------------------

   function atoi (A : System.Address) return Integer;
   pragma Import (C, atoi, "atoi");

   procedure clearerr (stream : FILEs);
   pragma Import (C, clearerr, "clearerr");

   function dup  (handle : int) return int;
   pragma Import (C, dup, "dup");

   function dup2 (from, to : int) return int;
   pragma Import (C, dup2, "dup2");

   function fclose (stream : FILEs) return int;
   pragma Import (C, fclose, "fclose");

   function fdopen (handle : int; mode : chars) return FILEs;
   pragma Import (C, fdopen, "fdopen");

   function fflush (stream : FILEs) return int;
   pragma Import (C, fflush, "fflush");

   function fgetc (stream : FILEs) return int;
   pragma Import (C, fgetc, "fgetc");

   function fgets (strng : chars; n : int; stream : FILEs) return chars;
   pragma Import (C, fgets, "fgets");

   function fopen
     (filename : chars;
      mode     : chars;
      encoding : Filename_Encoding := Unspecified) return FILEs;
   pragma Import (C, fopen, "__gnat_fopen");

   function fputc (C : int; stream : FILEs) return int;
   pragma Import (C, fputc, "fputc");

   function fputwc (C : int; stream : FILEs) return int;
   pragma Import (C, fputwc, "__gnat_fputwc");

   function fputs (Strng : chars; Stream : FILEs) return int;
   pragma Import (C, fputs, "fputs");

   procedure free (Ptr : System.Address);
   pragma Import (C, free, "free");

   function freopen
     (filename : chars;
      mode     : chars;
      stream   : FILEs;
      encoding : Filename_Encoding := Unspecified) return FILEs;
   pragma Import (C, freopen, "__gnat_freopen");

   function fseek
     (stream : FILEs;
      offset : long;
      origin : int) return int;
   pragma Import (C, fseek, "fseek");

   function fseek64
     (stream : FILEs;
      offset : int64;
      origin : int) return int;
   pragma Import (C, fseek64, "__gnat_fseek64");

   function ftell (stream : FILEs) return long;
   pragma Import (C, ftell, "ftell");

   function ftell64 (stream : FILEs) return int64;
   pragma Import (C, ftell64, "__gnat_ftell64");

   function getenv (S : String) return System.Address;
   pragma Import (C, getenv, "getenv");

   function isatty (handle : int) return int;
   pragma Import (C, isatty, "isatty");

   function lseek (fd : int; offset : off_t; direction : int) return off_t;
   pragma Import (C, lseek, "lseek");

   function malloc (Size : size_t) return System.Address;
   pragma Import (C, malloc, "malloc");

   procedure memcpy (S1 : System.Address; S2 : System.Address; N : size_t);
   pragma Import (C, memcpy, "memcpy");

   procedure memmove (S1 : System.Address; S2 : System.Address; N : size_t);
   pragma Import (C, memmove, "memmove");

   procedure mktemp (template : chars);
   pragma Import (C, mktemp, "mktemp");

   function pclose (stream : System.Address) return int;
   pragma Import (C, pclose, "pclose");

   function popen (command, mode : System.Address) return System.Address;
   pragma Import (C, popen, "popen");

   function realloc
     (Ptr : System.Address; Size : size_t) return System.Address;
   pragma Import (C, realloc, "realloc");

   procedure rewind (stream : FILEs);
   pragma Import (C, rewind, "rewind");

   function rmdir (dir_name : String) return int;
   pragma Import (C, rmdir, "__gnat_rmdir");

   function chdir (dir_name : String) return int;
   pragma Import (C, chdir, "__gnat_chdir");

   function mkdir
     (dir_name : String;
      encoding : Filename_Encoding := Unspecified) return int;
   pragma Import (C, mkdir, "__gnat_mkdir");

   function setvbuf
     (stream : FILEs;
      buffer : chars;
      mode   : int;
      size   : size_t) return int;
   pragma Import (C, setvbuf, "setvbuf");

   procedure tmpnam (str : chars);
   pragma Import (C, tmpnam, "tmpnam");

   function tmpfile return FILEs;
   pragma Import (C, tmpfile, "tmpfile");

   function ungetc (c : int; stream : FILEs) return int;
   pragma Import (C, ungetc, "ungetc");

   function unlink (filename : chars) return int;
   pragma Import (C, unlink, "__gnat_unlink");

   function open (filename : chars; oflag : int) return int;
   pragma Import (C, open, "__gnat_open");

   function close (fd : int) return int;
   pragma Import (C, close, "close");

   function read (fd : int; buffer : chars; count : size_t) return ssize_t;
   pragma Import (C, read, "read");

   function write (fd : int; buffer : chars; count : size_t) return ssize_t;
   pragma Import (C, write, "write");

end System.CRTL;

------------------------------------------------------------------------------
--                                                                          --
--                        GNAT RUN-TIME COMPONENTS                          --
--                                                                          --
--                         S Y S T E M . C R T L                            --
--                                                                          --
--                                S p e c                                   --
--                                                                          --
--         Copyright (C) 2004,2005 Free Software Foundation, Inc.           --
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

--  This package provides the low level interface to the C Run Time Library
--  on 64 bit VMS.  Note that routines that allocate memory remain 32bit.

with System.Parameters;
package System.CRTL is
pragma Preelaborate (CRTL);

   subtype chars is System.Address;
   --  Pointer to null-terminated array of characters

   subtype DIRs is System.Address;
   --  Corresponds to the C type DIR*

   subtype FILEs is System.Address;
   --  Corresponds to the C type FILE*

   subtype int is Integer;

   type long is range -(2 ** (System.Parameters.long_bits - 1))
      .. +(2 ** (System.Parameters.long_bits - 1)) - 1;

   subtype off_t is Integer;

   type size_t is mod 2 ** Standard'Address_Size;

   function atoi (A : System.Address) return Integer;
   pragma Import (C, atoi, "decc$atoi");

   procedure clearerr (stream : FILEs);
   pragma Import (C, clearerr, "decc$clearerr");

   function closedir (directory : DIRs) return Integer;
   pragma Import (C, closedir, "decc$closedir");

   function dup  (handle : int) return int;
   pragma Import (C, dup, "decc$dup");

   function dup2 (from, to : int) return int;
   pragma Import (C, dup2, "decc$dup2");

   function fclose (stream : FILEs) return int;
   pragma Import (C, fclose, "decc$fclose");

   function fdopen (handle : int; mode : chars) return FILEs;
   pragma Import (C, fdopen, "decc$fdopen");

   function fflush (stream : FILEs) return int;
   pragma Import (C, fflush, "decc$fflush");

   function fgetc (stream : FILEs) return int;
   pragma Import (C, fgetc, "decc$fgetc");

   function fgets (strng : chars; n : int; stream : FILEs) return chars;
   pragma Import (C, fgets, "decc$_fgets64");

   function fopen (filename : chars; Mode : chars) return FILEs;
   pragma Import (C, fopen, "decc$fopen");

   function fputc (C : int; stream : FILEs) return int;
   pragma Import (C, fputc, "decc$fputc");

   function fputs (Strng : chars; Stream : FILEs) return int;
   pragma Import (C, fputs, "decc$fputs");

   procedure free (Ptr : System.Address);
   pragma Import (C, free, "decc$free");

   function freopen
     (filename : chars;
      mode     : chars;
      stream   : FILEs)
      return     FILEs;
   pragma Import (C, freopen, "decc$freopen");

   function fseek
     (stream : FILEs;
      offset : long;
      origin : int)
      return   int;
   pragma Import (C, fseek, "decc$fseek");

   function ftell (stream : FILEs) return long;
   pragma Import (C, ftell, "decc$ftell");

   function getenv (S : String) return System.Address;
   pragma Import (C, getenv, "decc$getenv");

   function isatty (handle : int) return int;
   pragma Import (C, isatty, "decc$isatty");

   function lseek (fd : int; offset : off_t; direction : int) return off_t;
   pragma Import (C, lseek, "decc$lseek");

   function malloc (Size : size_t) return System.Address;
   pragma Import (C, malloc, "decc$malloc");

   procedure memcpy (S1 : System.Address; S2 : System.Address; N : size_t);
   pragma Import (C, memcpy, "decc$_memcpy64");

   procedure memmove (S1 : System.Address; S2 : System.Address; N : size_t);
   pragma Import (C, memmove, "decc$_memmove64");

   procedure mktemp (template : chars);
   pragma Import (C, mktemp, "decc$_mktemp64");

   function opendir (file_name : String) return DIRs;
   pragma Import (C, opendir, "decc$opendir");

   function pclose (stream : System.Address) return int;
   pragma Import (C, pclose, "decc$pclose");

   function popen (command, mode : System.Address) return System.Address;
   pragma Import (C, popen, "decc$popen");

   function read (fd : int; buffer : chars; nbytes : int) return int;
   pragma Import (C, read, "decc$read");

   function realloc
     (Ptr : System.Address; Size : size_t) return System.Address;
   pragma Import (C, realloc, "decc$realloc");

   procedure rewind (stream : FILEs);
   pragma Import (C, rewind, "decc$rewind");

   procedure rmdir (dir_name : String);
   pragma Import (C, rmdir, "decc$rmdir");

   function setvbuf
     (stream : FILEs;
      buffer : chars;
      mode   : int;
      size   : size_t)
      return   int;
   pragma Import (C, setvbuf, "decc$setvbuf");

   procedure tmpnam (string : chars);
   pragma Import (C, tmpnam, "decc$_tmpnam64");

   function tmpfile return FILEs;
   pragma Import (C, tmpfile, "decc$tmpfile");

   function ungetc (c : int; stream : FILEs) return int;
   pragma Import (C, ungetc, "decc$ungetc");

   function unlink (filename : chars) return int;
   pragma Import (C, unlink, "decc$unlink");

   function write (fd : int; buffer : chars; nbytes : int) return int;
   pragma Import (C, write, "decc$write");
end System.CRTL;

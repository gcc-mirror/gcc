------------------------------------------------------------------------------
--                                                                          --
--                        GNAT RUN-TIME COMPONENTS                          --
--                                                                          --
--                          S Y S T E M . C R T L                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--           Copyright (C) 2003 Free Software Foundation, Inc.              --
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
--  on non-VMS systems.

with System.Parameters;
package System.CRTL is
pragma Preelaborate (CRTL);

   subtype chars is System.Address;
   --  Pointer to null-terminated array of characters

   subtype FILEs is System.Address;
   --  Corresponds to the C type FILE*

   subtype int is Integer;

   type long is range -(2 ** (System.Parameters.long_bits - 1))
      .. +(2 ** (System.Parameters.long_bits - 1)) - 1;

   subtype off_t is Long_Integer;

   type size_t is mod 2 ** Standard'Address_Size;

   function atoi (A : System.Address) return Integer;
   pragma Import (C, atoi, "atoi");

   procedure clearerr (stream : FILEs);
   pragma Import (C, clearerr, "clearerr");

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

   function fopen (filename : chars; Mode : chars) return FILEs;
   pragma Import (C, fopen, "fopen");

   function fputc (C : int; stream : FILEs) return int;
   pragma Import (C, fputc, "fputc");

   function fputs (Strng : chars; Stream : FILEs) return int;
   pragma Import (C, fputs, "fputs");

   procedure free (Ptr : System.Address);
   pragma Import (C, free, "free");

   function freopen
     (filename : chars;
      mode     : chars;
      stream   : FILEs)
      return     FILEs;
   pragma Import (C, freopen, "freopen");

   function fseek
     (stream : FILEs;
      offset : long;
      origin : int)
      return   int;
   pragma Import (C, fseek, "fseek");

   function ftell (stream : FILEs) return long;
   pragma Import (C, ftell, "ftell");

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

   function read (fd : int; buffer : chars; nbytes : int) return int;
   pragma Import (C, read, "read");

   function realloc
     (Ptr : System.Address; Size : size_t) return System.Address;
   pragma Import (C, realloc, "realloc");

   procedure rewind (stream : FILEs);
   pragma Import (C, rewind, "rewind");

   function setvbuf
     (stream : FILEs;
      buffer : chars;
      mode   : int;
      size   : size_t)
      return   int;
   pragma Import (C, setvbuf, "setvbuf");

   procedure tmpnam (string : chars);
   pragma Import (C, tmpnam, "tmpnam");

   function tmpfile return FILEs;
   pragma Import (C, tmpfile, "tmpfile");

   function ungetc (c : int; stream : FILEs) return int;
   pragma Import (C, ungetc, "ungetc");

   function unlink (filename : chars) return int;
   pragma Import (C, unlink, "unlink");

   function write (fd : int; buffer : chars; nbytes : int) return int;
   pragma Import (C, write, "write");
end System.CRTL;

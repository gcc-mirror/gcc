------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                          G N A T . R E G P A T                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--               Copyright (C) 1986 by University of Toronto.               --
--                     Copyright (C) 1996-2007, AdaCore                     --
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

--  This package implements roughly the same set of regular expressions as
--  are available in the Perl or Python programming languages.

--  This is an extension of the original V7 style regular expression library
--  written in C by Henry Spencer. Apart from the translation to Ada, the
--  interface has been considerably changed to use the Ada String type
--  instead of C-style nul-terminated strings.

--  See file s-regpat.ads for full documentation of the interface

------------------------------------------------------------
-- Summary of Pattern Matching Packages in GNAT Hierarchy --
------------------------------------------------------------

--  There are three related packages that perform pattern maching functions.
--  the following is an outline of these packages, to help you determine
--  which is best for your needs.

--     GNAT.Regexp (files g-regexp.ads/s-regexp.ads/s-regexp.adb)
--       This is a simple package providing Unix-style regular expression
--       matching with the restriction that it matches entire strings. It
--       is particularly useful for file name matching, and in particular
--       it provides "globbing patterns" that are useful in implementing
--       unix or DOS style wild card matching for file names.

--     GNAT.Regpat (files g-regpat.ads/s-regpat.ads/s-regpat.adb)
--       This is a more complete implementation of Unix-style regular
--       expressions, copied from the Perl regular expression engine,
--       written originally in C by Henry Spencer. It is functionally the
--       same as that library.

--     GNAT.Spitbol.Patterns (files g-spipat.ads/g-spipat.adb)
--       This is a completely general pattern matching package based on the
--       pattern language of SNOBOL4, as implemented in SPITBOL. The pattern
--       language is modeled on context free grammars, with context sensitive
--       extensions that provide full (type 0) computational capabilities.

with System.Regpat;

package GNAT.Regpat renames System.Regpat;

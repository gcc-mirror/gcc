------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                          G N A T . R E G E X P                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 1998-2019, AdaCore                     --
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

--  Simple Regular expression matching

--  This package provides a simple implementation of a regular expression
--  pattern matching algorithm, using a subset of the syntax of regular
--  expressions copied from familiar Unix style utilities.

--  See file s-regexp.ads for full documentation of the interface

------------------------------------------------------------
-- Summary of Pattern Matching Packages in GNAT Hierarchy --
------------------------------------------------------------

--  There are three related packages that perform pattern matching functions.
--  the following is an outline of these packages, to help you determine
--  which is best for your needs.

--     GNAT.Regexp (files g-regexp.ads/s-regexp.ads/s-regexp.adb)
--       This is a simple package providing Unix-style regular expression
--       matching with the restriction that it matches entire strings. It
--       is particularly useful for file name matching, and in particular
--       it provides "globbing patterns" that are useful in implementing
--       unix or DOS style wildcard matching for file names.

--     GNAT.Regpat (files g-regpat.ads/s-regpat.ads/g-regpat.adb)
--       This is a more complete implementation of Unix-style regular
--       expressions, copied from the original V7 style regular expression
--       library written in C by Henry Spencer. It is functionally the
--       same as this library, and uses the same internal data structures
--       stored in a binary compatible manner.

--     GNAT.Spitbol.Patterns (files g-spipat.ads/g-spipat.adb)
--       This is a completely general pattern matching package based on the
--       pattern language of SNOBOL4, as implemented in SPITBOL. The pattern
--       language is modeled on context free grammars, with context sensitive
--       extensions that provide full (type 0) computational capabilities.

with System.Regexp;

package GNAT.Regexp renames System.Regexp;

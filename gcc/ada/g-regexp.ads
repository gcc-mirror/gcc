------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                          G N A T . R E G E X P                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.12 $
--                                                                          --
--           Copyright (C) 1998-1999 Ada Core Technologies, Inc.            --
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
-- GNAT is maintained by Ada Core Technologies Inc (http://www.gnat.com).   --
--                                                                          --
------------------------------------------------------------------------------

--  Simple Regular expression matching

--  This package provides a simple implementation of a regular expression
--  pattern matching algorithm, using a subset of the syntax of regular
--  expressions copied from familiar Unix style utilities.

------------------------------------------------------------
-- Summary of Pattern Matching Packages in GNAT Hierarchy --
------------------------------------------------------------

--  There are three related packages that perform pattern maching functions.
--  the following is an outline of these packages, to help you determine
--  which is best for your needs.

--     GNAT.Regexp (files g-regexp.ads/g-regexp.adb)
--       This is a simple package providing Unix-style regular expression
--       matching with the restriction that it matches entire strings. It
--       is particularly useful for file name matching, and in particular
--       it provides "globbing patterns" that are useful in implementing
--       unix or DOS style wild card matching for file names.

--     GNAT.Regpat (files g-regpat.ads/g-regpat.adb)
--       This is a more complete implementation of Unix-style regular
--       expressions, copied from the original V7 style regular expression
--       library written in C by Henry Spencer. It is functionally the
--       same as this library, and uses the same internal data structures
--       stored in a binary compatible manner.

--     GNAT.Spitbol.Patterns (files g-spipat.ads/g-spipat.adb)
--       This is a completely general patterm matching package based on the
--       pattern language of SNOBOL4, as implemented in SPITBOL. The pattern
--       language is modeled on context free grammars, with context sensitive
--       extensions that provide full (type 0) computational capabilities.

with Ada.Finalization;

package GNAT.Regexp is

   --  The regular expression must first be compiled, using the Compile
   --  function, which creates a finite state matching table, allowing
   --  very fast matching once the expression has been compiled.

   --  The following is the form of a regular expression, expressed in Ada
   --  reference manual style BNF is as follows

   --     regexp ::= term

   --     regexp ::= term | term          -- alternation (term or term ...)

   --     term ::= item

   --     term ::= item item ...          -- concatenation (item then item)

   --     item ::= elmt                   -- match elmt
   --     item ::= elmt *                 -- zero or more elmt's
   --     item ::= elmt +                 -- one or more elmt's
   --     item ::= elmt ?                 -- matches elmt or nothing

   --     elmt ::= nchr                   -- matches given character
   --     elmt ::= [nchr nchr ...]        -- matches any character listed
   --     elmt ::= [^ nchr nchr ...]      -- matches any character not listed
   --     elmt ::= [char - char]          -- matches chars in given range
   --     elmt ::= .                      -- matches any single character
   --     elmt ::= ( regexp )             -- parens used for grouping

   --     char ::= any character, including special characters
   --     nchr ::= any character except \()[].*+?^ or \char to match char
   --     ... is used to indication repetition (one or more terms)

   --  See also regexp(1) man page on Unix systems for further details

   --  A second kind of regular expressions is provided. This one is more
   --  like the wild card patterns used in file names by the Unix shell (or
   --  DOS prompt) command lines. The grammar is the following:

   --     regexp ::= term

   --     term   ::= elmt

   --     term   ::= elmt elmt ...     -- concatenation (elmt then elmt)
   --     term   ::= *                 -- any string of 0 or more characters
   --     term   ::= ?                 -- matches any character
   --     term   ::= [char char ...]   -- matches any character listed
   --     term   ::= [char - char]     -- matches any character in given range
   --     term   ::= {elmt, elmt, ...} -- alternation (matches any of elmt)

   --  Important note : This package was mainly intended to match regular
   --  expressions against file names. The whole string has to match the
   --  regular expression. If only a substring matches, then the function
   --  Match will return False.

   type Regexp is private;
   --  Private type used to represent a regular expression

   Error_In_Regexp : exception;
   --  Exception raised when an error is found in the regular expression

   function Compile
     (Pattern        : String;
      Glob           : Boolean := False;
      Case_Sensitive : Boolean := True)
      return           Regexp;
   --  Compiles a regular expression S. If the syntax of the given
   --  expression is invalid (does not match above grammar, Error_In_Regexp
   --  is raised. If Glob is True, the pattern is considered as a 'globbing
   --  pattern', that is a pattern as given by the second grammar above

   function Match (S : String; R : Regexp) return Boolean;
   --  True if S matches R, otherwise False. Raises Constraint_Error if
   --  R is an uninitialized regular expression value.

private
   type Regexp_Value;

   type Regexp_Access is access Regexp_Value;

   type Regexp is new Ada.Finalization.Controlled with record
      R : Regexp_Access := null;
   end record;

   pragma Finalize_Storage_Only (Regexp);

   procedure Finalize (R : in out Regexp);
   --  Free the memory occupied by R

   procedure Adjust (R : in out Regexp);
   --  Called after an assignment (do a copy of the Regexp_Access.all)

end GNAT.Regexp;

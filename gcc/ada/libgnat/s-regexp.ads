------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                        S Y S T E M . R E G E X P                         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 1998-2021, AdaCore                     --
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

--  Note: this package is in the System hierarchy so that it can be directly
--  be used by other predefined packages. User access to this package is via
--  a renaming of this package in GNAT.Regexp (file g-regexp.ads).

with Ada.Finalization;

package System.Regexp is

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
   --  like the wildcard patterns used in file names by the Unix shell (or
   --  DOS prompt) command lines. The grammar is the following:

   --     regexp ::= term

   --     term   ::= elmt
   --     term   ::= seq
   --     term   ::= {seq, seq, ...}   -- alternation (matches any of seq)

   --     seq    ::= elmt elmt ...     -- concatenation (sequence of elmts)

   --     elmt   ::= *                 -- any string of 0 or more characters
   --     elmt   ::= ?                 -- matches any character
   --     elmt   ::= char
   --     elmt   ::= [^ char char ...] -- matches any character not listed
   --     elmt   ::= [char char ...]   -- matches any character listed
   --     elmt   ::= [char - char]     -- matches any character in given range

   --     \char is also supported by this grammar.

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
      Case_Sensitive : Boolean := True) return Regexp;
   --  Compiles a regular expression S. If the syntax of the given
   --  expression is invalid (does not match above grammar), Error_In_Regexp
   --  is raised. If Glob is True, the pattern is considered as a 'globbing
   --  pattern', that is a pattern as given by the second grammar above.
   --  As a special case, if Pattern is the empty string it will always
   --  match.

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

end System.Regexp;

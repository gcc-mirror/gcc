------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                          G N A T . R E G P A T                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--               Copyright (C) 1986 by University of Toronto.               --
--           Copyright (C) 1996-2003 Ada Core Technologies, Inc.            --
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

--  This package implements roughly the same set of regular expressions as
--  are available in the Perl or Python programming languages.

--  This is an extension of the original V7 style regular expression library
--  written in C by Henry Spencer. Apart from the translation to Ada, the
--  interface has been considerably changed to use the Ada String type
--  instead of C-style nul-terminated strings.

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
--       expressions, copied from the Perl regular expression engine,
--       written originally in C by Henry Spencer. It is functionally the
--       same as that library.

--     GNAT.Spitbol.Patterns (files g-spipat.ads/g-spipat.adb)
--       This is a completely general pattern matching package based on the
--       pattern language of SNOBOL4, as implemented in SPITBOL. The pattern
--       language is modeled on context free grammars, with context sensitive
--       extensions that provide full (type 0) computational capabilities.

package GNAT.Regpat is
pragma Preelaborate (Regpat);

   --  The grammar is the following:

   --     regexp ::= expr
   --            ::= ^ expr               -- anchor at the beginning of string
   --            ::= expr $               -- anchor at the end of string

   --     expr   ::= term
   --            ::= term | term          -- alternation (term or term ...)

   --     term   ::= item
   --            ::= item item ...        -- concatenation (item then item)

   --     item   ::= elmt                 -- match elmt
   --            ::= elmt *               -- zero or more elmt's
   --            ::= elmt +               -- one or more elmt's
   --            ::= elmt ?               -- matches elmt or nothing
   --            ::= elmt *?              -- zero or more times, minimum number
   --            ::= elmt +?              -- one or more times, minimum number
   --            ::= elmt ??              -- zero or one time, minimum number
   --            ::= elmt { num }         -- matches elmt exactly num times
   --            ::= elmt { num , }       -- matches elmt at least num times
   --            ::= elmt { num , num2 }  -- matches between num and num2 times
   --            ::= elmt { num }?        -- matches elmt exactly num times
   --            ::= elmt { num , }?      -- matches elmt at least num times
   --                                        non-greedy version
   --            ::= elmt { num , num2 }? -- matches between num and num2 times
   --                                        non-greedy version

   --     elmt   ::= nchr                 -- matches given character
   --            ::= [range range ...]    -- matches any character listed
   --            ::= [^ range range ...]  -- matches any character not listed
   --            ::= .                    -- matches any single character
   --                                     -- except newlines
   --            ::= ( expr )             -- parens used for grouping
   --            ::= \ num                -- reference to num-th parenthesis

   --     range  ::= char - char          -- matches chars in given range
   --            ::= nchr
   --            ::= [: posix :]          -- any character in the POSIX range
   --            ::= [:^ posix :]         -- not in the POSIX range

   --     posix  ::= alnum                -- alphanumeric characters
   --            ::= alpha                -- alphabetic characters
   --            ::= ascii                -- ascii characters (0 .. 127)
   --            ::= cntrl                -- control chars (0..31, 127..159)
   --            ::= digit                -- digits ('0' .. '9')
   --            ::= graph                -- graphic chars (32..126, 160..255)
   --            ::= lower                -- lower case characters
   --            ::= print                -- printable characters (32..127)
   --            ::= punct                -- printable, except alphanumeric
   --            ::= space                -- space characters
   --            ::= upper                -- upper case characters
   --            ::= word                 -- alphanumeric characters
   --            ::= xdigit               -- hexadecimal chars (0..9, a..f)

   --     char   ::= any character, including special characters
   --                ASCII.NUL is not supported.

   --     nchr   ::= any character except \()[].*+?^ or \char to match char
   --                \n means a newline (ASCII.LF)
   --                \t means a tab (ASCII.HT)
   --                \r means a return (ASCII.CR)
   --                \b matches the empty string at the beginning or end of a
   --                   word. A word is defined as a set of alphanumerical
   --                   characters (see \w below).
   --                \B matches the empty string only when *not* at the
   --                   beginning or end of a word.
   --                \d matches any digit character ([0-9])
   --                \D matches any non digit character ([^0-9])
   --                \s matches any white space character. This is equivalent
   --                   to [ \t\n\r\f\v]  (tab, form-feed, vertical-tab,...
   --                \S matches any non-white space character.
   --                \w matches any alphanumeric character or underscore.
   --                   This include accented letters, as defined in the
   --                   package Ada.Characters.Handling.
   --                \W matches any non-alphanumeric character.
   --                \A match the empty string only at the beginning of the
   --                   string, whatever flags are used for Compile (the
   --                   behavior of ^ can change, see Regexp_Flags below).
   --                \G match the empty string only at the end of the
   --                   string, whatever flags are used for Compile (the
   --                   behavior of $ can change, see Regexp_Flags below).
   --     ...    ::= is used to indication repetition (one or more terms)

   --  Embedded newlines are not matched by the ^ operator.
   --  It is possible to retrieve the substring matched a parenthesis
   --  expression. Although the depth of parenthesis is not limited in the
   --  regexp, only the first 9 substrings can be retrieved.

   --  The highest value possible for the arguments to the curly operator ({})
   --  are given by the constant Max_Curly_Repeat below.

   --  The operators '*', '+', '?' and '{}' always match the longest possible
   --  substring. They all have a non-greedy version (with an extra ? after the
   --  operator), which matches the shortest possible substring.

   --  For instance:
   --      regexp="<.*>"   string="<h1>title</h1>"   matches="<h1>title</h1>"
   --      regexp="<.*?>"  string="<h1>title</h1>"   matches="<h1>"
   --
   --  '{' and '}' are only considered as special characters if they appear
   --  in a substring that looks exactly like '{n}', '{n,m}' or '{n,}', where
   --  n and m are digits. No space is allowed. In other contexts, the curly
   --  braces will simply be treated as normal characters.

   --  Compiling Regular Expressions
   --  =============================

   --  To use this package, you first need to compile the regular expression
   --  (a string) into a byte-code program, in a Pattern_Matcher structure.
   --  This first step checks that the regexp is valid, and optimizes the
   --  matching algorithms of the second step.

   --  Two versions of the Compile subprogram are given: one in which this
   --  package will compute itself the best possible size to allocate for the
   --  byte code; the other where you must allocate enough memory yourself. An
   --  exception is raised if there is not enough memory.

   --     declare
   --        Regexp : String := "a|b";

   --        Matcher : Pattern_Matcher := Compile (Regexp);
   --        --  The size for matcher is automatically allocated

   --        Matcher2 : Pattern_Matcher (1000);
   --        --  Some space is allocated directly.

   --     begin
   --        Compile (Matcher2, Regexp);
   --        ...
   --     end;

   --  Note that the second version is significantly faster, since with the
   --  first version the regular expression has in fact to be compiled twice
   --  (first to compute the size, then to generate the byte code).

   --  Note also that you can not use the function version of Compile if you
   --  specify the size of the Pattern_Matcher, since the discriminants will
   --  most probably be different and you will get a Constraint_Error

   --  Matching Strings
   --  ================

   --  Once the regular expression has been compiled, you can use it as often
   --  as needed to match strings.

   --  Several versions of the Match subprogram are provided, with different
   --  parameters and return results.

   --  See the description under each of these subprograms.

   --  Here is a short example showing how to get the substring matched by
   --  the first parenthesis pair.

   --     declare
   --        Matches : Match_Array (0 .. 1);
   --        Regexp  : String := "a(b|c)d";
   --        Str     : String := "gacdg";

   --     begin
   --        Match (Compile (Regexp), Str, Matches);
   --        return Str (Matches (1).First .. Matches (1).Last);
   --        --  returns 'c'
   --     end;

   --  Finding all occurrences
   --  =======================

   --  Finding all the occurrences of a regular expression in a string cannot
   --  be done by simply passing a slice of the string. This wouldn't work for
   --  anchored regular expressions (the ones starting with "^" or ending with
   --  "$").
   --  Instead, you need to use the last parameter to Match (Data_First), as in
   --  the following loop:

   --     declare
   --        Str     : String :=
   --           "-- first line" & ASCII.LF & "-- second line";
   --        Matches : Match_array (0 .. 0);
   --        Regexp  : Pattern_Matcher := Compile ("^--", Multiple_Lines);
   --        Current : Natural := Str'First;
   --     begin
   --        loop
   --           Match (Regexp, Str, Matches, Current);
   --           exit when Matches (0) = No_Match;
   --
   --           --  Process the match at position Matches (0).First
   --
   --           Current := Matches (0).Last + 1;
   --        end loop;
   --     end;

   --  String Substitution
   --  ===================

   --  No subprogram is currently provided for string substitution.
   --  However, this is easy to simulate with the parenthesis groups, as
   --  shown below.

   --  This example swaps the first two words of the string:

   --     declare
   --        Regexp  : String := "([a-z]+) +([a-z]+)";
   --        Str     : String := " first   second third ";
   --        Matches : Match_Array (0 .. 2);

   --     begin
   --        Match (Compile (Regexp), Str, Matches);
   --        return Str (Str'First .. Matches (1).First - 1)
   --               & Str (Matches (2).First .. Matches (2).Last)
   --               & " "
   --               & Str (Matches (1).First .. Matches (1).Last)
   --               & Str (Matches (2).Last + 1 .. Str'Last);
   --        --  returns " second first third "
   --     end;

   ---------------
   -- Constants --
   ---------------

   Expression_Error : exception;
   --  This exception is raised when trying to compile an invalid
   --  regular expression. All subprograms taking an expression
   --  as parameter may raise Expression_Error.

   Max_Paren_Count : constant := 255;
   --  Maximum number of parenthesis in a regular expression.
   --  This is limited by the size of a Character, as found in the
   --  byte-compiled version of regular expressions.

   Max_Program_Size : constant := 2**15 - 1;
   --  Maximum size that can be allocated for a program

   Max_Curly_Repeat : constant := 32767;
   --  Maximum number of repetition for the curly operator.
   --  The digits in the {n}, {n,} and {n,m } operators can not be higher
   --  than this constant, since they have to fit on two characters in the
   --  byte-compiled version of regular expressions.

   type Program_Size is range 0 .. Max_Program_Size;
   for Program_Size'Size use 16;
   --  Number of bytes allocated for the byte-compiled version of a regular
   --  expression.

   type Regexp_Flags is mod 256;
   for Regexp_Flags'Size use 8;
   --  Flags that can be given at compile time to specify default
   --  properties for the regular expression.

   No_Flags         : constant Regexp_Flags;
   Case_Insensitive : constant Regexp_Flags;
   --  The automaton is optimized so that the matching is done in a case
   --  insensitive manner (upper case characters and lower case characters
   --  are all treated the same way).

   Single_Line      : constant Regexp_Flags;
   --  Treat the Data we are matching as a single line. This means that
   --  ^ and $ will ignore \n (unless Multiple_Lines is also specified),
   --  and that '.' will match \n.

   Multiple_Lines   : constant Regexp_Flags;
   --  Treat the Data as multiple lines. This means that ^ and $ will also
   --  match on internal newlines (ASCII.LF), in addition to the beginning
   --  and end of the string.
   --
   --  This can be combined with Single_Line.

   -----------------
   -- Match_Array --
   -----------------

   subtype Match_Count is Natural range 0 .. Max_Paren_Count;

   type Match_Location is record
      First : Natural := 0;
      Last  : Natural := 0;
   end record;

   type Match_Array is array (Match_Count range <>) of Match_Location;
   --  The substring matching a given pair of parenthesis.
   --  Index 0 is the whole substring that matched the full regular
   --  expression.
   --
   --  For instance, if your regular expression is something like:
   --  "a(b*)(c+)", then Match_Array(1) will be the indexes of the
   --  substring that matched "b*" and Match_Array(2) will be the substring
   --  that matched "c+".
   --
   --  The number of parenthesis groups that can be retrieved is unlimited,
   --  and all the Match subprograms below can use a Match_Array of any size.
   --  Indexes that do not have any matching parenthesis are set to
   --  No_Match.

   No_Match : constant Match_Location := (First => 0, Last => 0);
   --  The No_Match constant is (0, 0) to differentiate between
   --  matching a null string at position 1, which uses (1, 0)
   --  and no match at all.

   ------------------------------
   -- Pattern_Matcher Creation --
   ------------------------------

   type Pattern_Matcher (Size : Program_Size) is private;
   --  Type used to represent a regular expression compiled into byte code

   Never_Match : constant Pattern_Matcher;
   --  A regular expression that never matches anything

   function Compile
     (Expression : String;
      Flags      : Regexp_Flags := No_Flags) return Pattern_Matcher;
   --  Compile a regular expression into internal code.
   --  Raises Expression_Error if Expression is not a legal regular expression.
   --  The appropriate size is calculated automatically, but this means that
   --  the regular expression has to be compiled twice (the first time to
   --  calculate the size, the second time to actually generate the byte code).
   --
   --  Flags is the default value to use to set properties for Expression (case
   --  sensitivity,...).

   procedure Compile
     (Matcher         : out Pattern_Matcher;
      Expression      : String;
      Final_Code_Size : out Program_Size;
      Flags           : Regexp_Flags := No_Flags);
   --  Compile a regular expression into into internal code
   --  This procedure is significantly faster than the function
   --  Compile, as there is a known maximum size for the matcher.
   --  This function raises Storage_Error if Matcher is too small
   --  to hold the resulting code, or Expression_Error is Expression
   --  is not a legal regular expression.
   --
   --  Flags is the default value to use to set properties for Expression (case
   --  sensitivity,...).

   procedure Compile
     (Matcher    : out Pattern_Matcher;
      Expression : String;
      Flags      : Regexp_Flags := No_Flags);
   --  Same procedure as above, expect it does not return the final
   --  program size.

   function Paren_Count (Regexp : Pattern_Matcher) return Match_Count;
   pragma Inline (Paren_Count);
   --  Return the number of parenthesis pairs in Regexp.
   --
   --  This is the maximum index that will be filled if a Match_Array is
   --  used as an argument to Match.
   --
   --  Thus, if you want to be sure to get all the parenthesis, you should
   --  do something like:
   --
   --     declare
   --        Regexp  : Pattern_Matcher := Compile ("a(b*)(c+)");
   --        Matched : Match_Array (0 .. Paren_Count (Regexp));
   --     begin
   --        Match (Regexp, "a string", Matched);
   --     end;

   -------------
   -- Quoting --
   -------------

   function Quote (Str : String) return String;
   --  Return a version of Str so that every special character is quoted.
   --  The resulting string can be used in a regular expression to match
   --  exactly Str, whatever character was present in Str.

   --------------
   -- Matching --
   --------------

   procedure Match
     (Expression     : String;
      Data           : String;
      Matches        : out Match_Array;
      Size           : Program_Size := 0;
      Data_First     : Integer      := -1;
      Data_Last      : Positive     := Positive'Last);
   --  Match Expression against Data (Data_First .. Data_Last) and store
   --  result in Matches.
   --
   --  Data_First defaults to Data'First if unspecified (that is the
   --  dummy value of -1 is interpreted to mean Data'First).
   --
   --  Data_Last defaults to Data'Last if unspecified (that is the
   --  dummy value of Positive'Last is interpreted to mean Data'Last)
   --
   --  It is important that Data contains the whole string (or file) you
   --  want to matched against, even if you start in the middle, since
   --  otherwise regular expressions starting with "^" or ending with "$" will
   --  be improperly processed.
   --
   --  Function raises Storage_Error if Size is too small for Expression,
   --  or Expression_Error if Expression is not a legal regular expression.
   --  If Size is 0, then the appropriate size is automatically calculated
   --  by this package, but this is slightly slower.
   --
   --  At most Matches'Length parenthesis are returned.

   function  Match
     (Expression : String;
      Data       : String;
      Size       : Program_Size := 0;
      Data_First : Integer  := -1;
      Data_Last  : Positive := Positive'Last) return Natural;
   --  Return the position where Data matches, or (Data'First - 1) if
   --  there is no match.
   --
   --  Function raises Storage_Error if Size is too small for Expression
   --  or Expression_Error if Expression is not a legal regular expression
   --
   --  If Size is 0, then the appropriate size is automatically calculated
   --  by this package, but this is slightly slower.
   --  See description of Data_First and Data_Last above.

   function Match
     (Expression : String;
      Data       : String;
      Size       : Program_Size := 0;
      Data_First : Integer  := -1;
      Data_Last  : Positive := Positive'Last) return Boolean;
   --  Return True if Data matches Expression. Match raises Storage_Error
   --  if Size is too small for Expression, or Expression_Error if Expression
   --  is not a legal regular expression.
   --
   --  If Size is 0, then the appropriate size is automatically calculated
   --  by this package, but this is slightly slower.
   --
   --  See description of Data_First and Data_Last above.

   ------------------------------------------------
   -- Matching a pre-compiled regular expression --
   ------------------------------------------------

   --  The following functions are significantly faster if you need to reuse
   --  the same regular expression multiple times, since you only have to
   --  compile it once.

   function  Match
     (Self       : Pattern_Matcher;
      Data       : String;
      Data_First : Integer  := -1;
      Data_Last  : Positive := Positive'Last) return Natural;
   --  Match Data using the given pattern matcher.
   --  Return the position where Data matches, or (Data'First - 1) if there is
   --  no match.
   --
   --  See description of Data_First and Data_Last above.

   function  Match
     (Self       : Pattern_Matcher;
      Data       : String;
      Data_First : Integer  := -1;
      Data_Last  : Positive := Positive'Last) return Boolean;
   --  Return True if Data matches using the given pattern matcher.
   --
   --  See description of Data_First and Data_Last above.

   pragma Inline (Match);
   --  All except the last one below

   procedure Match
     (Self       : Pattern_Matcher;
      Data       : String;
      Matches    : out Match_Array;
      Data_First : Integer  := -1;
      Data_Last  : Positive := Positive'Last);
   --  Match Data using the given pattern matcher and store result in Matches.
   --  The expression matches if Matches (0) /= No_Match.
   --
   --  At most Matches'Length parenthesis are returned.
   --
   --  See description of Data_First and Data_Last above.

   -----------
   -- Debug --
   -----------

   procedure Dump (Self : Pattern_Matcher);
   --  Dump the compiled version of the regular expression matched by Self

--------------------------
-- Private Declarations --
--------------------------

private

   subtype Pointer is Program_Size;
   --  The Pointer type is used to point into Program_Data

   --  Note that the pointer type is not necessarily 2 bytes
   --  although it is stored in the program using 2 bytes

   type Program_Data is array (Pointer range <>) of Character;

   Program_First : constant := 1;

   --  The "internal use only" fields in regexp are present to pass
   --  info from compile to execute that permits the execute phase
   --  to run lots faster on simple cases.  They are:

   --     First              character that must begin a match or ASCII.Nul
   --     Anchored           true iff match must start at beginning of line
   --     Must_Have          pointer to string that match must include or null
   --     Must_Have_Length   length of Must_Have string

   --  First and Anchored permit very fast decisions on suitable
   --  starting points for a match, cutting down the work a lot.
   --  Must_Have permits fast rejection of lines that cannot possibly
   --  match.

   --  The Must_Have tests are costly enough that Optimize
   --  supplies a Must_Have only if the r.e. contains something potentially
   --  expensive (at present, the only such thing detected is * or +
   --  at the start of the r.e., which can involve a lot of backup).
   --  The length is supplied because the test in Execute needs it
   --  and Optimize is computing it anyway.

   --  The initialization is meant to fail-safe in case the user of this
   --  package tries to use an uninitialized matcher. This takes advantage
   --  of the knowledge that ASCII.Nul translates to the end-of-program (EOP)
   --  instruction code of the state machine.

   No_Flags         : constant Regexp_Flags := 0;
   Case_Insensitive : constant Regexp_Flags := 1;
   Single_Line      : constant Regexp_Flags := 2;
   Multiple_Lines   : constant Regexp_Flags := 4;

   type Pattern_Matcher (Size : Pointer) is record
      First            : Character    := ASCII.NUL;  --  internal use only
      Anchored         : Boolean      := False;      --  internal use only
      Must_Have        : Pointer      := 0;          --  internal use only
      Must_Have_Length : Natural      := 0;          --  internal use only
      Paren_Count      : Natural      := 0;          --  # paren groups
      Flags            : Regexp_Flags := No_Flags;
      Program          : Program_Data (Program_First .. Size) :=
                           (others => ASCII.NUL);
   end record;

   Never_Match : constant Pattern_Matcher :=
      (0, ASCII.NUL, False, 0, 0, 0, No_Flags, (others => ASCII.NUL));

end GNAT.Regpat;

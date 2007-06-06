------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                        S Y S T E M . R E G P A T                         --
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

--  Note: this package is in the System hierarchy so that it can be directly
--  be used by other predefined packages. User access to this package is via
--  a renaming of this package in GNAT.Regpat (file g-regpat.ads).

package System.Regpat is
   pragma Preelaborate;

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
   --                                     -- and whitespaces (9 .. 13)
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

   --  Note also that you cannot use the function version of Compile if you
   --  specify the size of the Pattern_Matcher, since the discriminants will
   --  most probably be different and you will get a Constraint_Error

   --  Matching Strings
   --  ================

   --  Once the regular expression has been compiled, you can use it as often
   --  as needed to match strings.

   --  Several versions of the Match subprogram are provided, with different
   --  parameters and return results.

   --  See the description under each of these subprograms

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
   --        Matches : Match_Array (0 .. 0);
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
   --  This exception is raised when trying to compile an invalid regular
   --  expression. All subprograms taking an expression as parameter may raise
   --  Expression_Error.

   Max_Paren_Count : constant := 255;
   --  Maximum number of parenthesis in a regular expression. This is limited
   --  by the size of a Character, as found in the byte-compiled version of
   --  regular expressions.

   Max_Curly_Repeat : constant := 32767;
   --  Maximum number of repetition for the curly operator. The digits in the
   --  {n}, {n,} and {n,m } operators cannot be higher than this constant,
   --  since they have to fit on two characters in the byte-compiled version of
   --  regular expressions.

   Max_Program_Size : constant := 2**15 - 1;
   --  Maximum size that can be allocated for a program

   type Program_Size is range 0 .. Max_Program_Size;
   for Program_Size'Size use 16;
   --  Number of bytes allocated for the byte-compiled version of a regular
   --  expression. The size required depends on the complexity of the regular
   --  expression in a complex manner that is undocumented (other than in the
   --  body of the Compile procedure). Normally the size is automatically set
   --  and the programmer need not be concerned about it. There are two
   --  exceptions to this. First in the calls to Match, it is possible to
   --  specify a non-zero size that is known to be large enough. This can
   --  slightly increase the efficiency by avoiding a copy. Second, in the case
   --  of calling compile, it is possible using the procedural form of Compile
   --  to use a single Pattern_Matcher variable for several different
   --  expressions by setting its size sufficiently large.

   Auto_Size : constant := 0;
   --  Used in calls to Match to indicate that the Size should be set to
   --  a value appropriate to the expression being used automatically.

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
   --  Used for regular expressions that can contain parenthesized
   --  subexpressions. Certain Match subprograms below produce Matches of type
   --  Match_Array. Each component of Matches is set to the subrange of the
   --  matches substring, or to No_Match if no match. Matches (N) is for the
   --  N'th parenthesized subexpressions; Matches (0) is for the whole
   --  expression.
   --
   --  For instance, if your regular expression is: "a((b*)c+)(d+)", then
   --                                                 12      3
   --     Matches (0) is for "a((b*)c+)(d+)" (the entire expression)
   --     Matches (1) is for "(b*)c+"
   --     Matches (2) is for "c+"
   --     Matches (3) is for "d+"
   --
   --  The number of parenthesis groups that can be retrieved is limited only
   --  by Max_Paren_Count.
   --
   --  Normally, the bounds of the Matches actual parameter will be
   --  0 .. Paren_Count (Regexp), to get all the matches. However, it is fine
   --  if Matches is shorter than that on either end; missing components will
   --  be ignored. Thus, in the above example, you could use 2 .. 2 if all you
   --  care about it the second parenthesis pair "b*". Likewise, if
   --  Matches'Last > Paren_Count (Regexp), the extra components will be set to
   --  No_Match.

   No_Match : constant Match_Location := (First => 0, Last => 0);
   --  The No_Match constant is (0, 0) to differentiate between matching a null
   --  string at position 1, which uses (1, 0) and no match at all.

   ---------------------------------
   -- Pattern_Matcher Compilation --
   ---------------------------------

   --  The subprograms here are used to precompile regular expressions for use
   --  in subsequent Match calls. Precompilation improves efficiency if the
   --  same regular expression is to be used in more than one Match call.

   type Pattern_Matcher (Size : Program_Size) is private;
   --  Type used to represent a regular expression compiled into byte code

   Never_Match : constant Pattern_Matcher;
   --  A regular expression that never matches anything

   function Compile
     (Expression : String;
      Flags      : Regexp_Flags := No_Flags) return Pattern_Matcher;
   --  Compile a regular expression into internal code
   --
   --  Raises Expression_Error if Expression is not a legal regular expression
   --
   --  The appropriate size is calculated automatically to correspond to the
   --  provided expression. This is the normal default method of compilation.
   --  Note that it is generally not possible to assign the result of two
   --  different calls to this Compile function to the same Pattern_Matcher
   --  variable, since the sizes will differ.
   --
   --  Flags is the default value to use to set properties for Expression
   --  (e.g. case sensitivity,...).

   procedure Compile
     (Matcher         : out Pattern_Matcher;
      Expression      : String;
      Final_Code_Size : out Program_Size;
      Flags           : Regexp_Flags := No_Flags);
   --  Compile a regular expression into into internal code

   --  This procedure is significantly faster than the Compile function since
   --  it avoids the extra step of precomputing the required size.
   --
   --  However, it requires the user to provide a Pattern_Matcher variable
   --  whose size is preset to a large enough value. One advantage of this
   --  approach, in addition to the improved efficiency, is that the same
   --  Pattern_Matcher variable can be used to hold the compiled code for
   --  several different regular expressions by setting a size that is large
   --  enough to accomodate all possibilities.
   --
   --  In this version of the procedure call, the actual required code size is
   --  returned. Also if Matcher.Size is zero on entry, then the resulting code
   --  is not stored. A call with Matcher.Size set to Auto_Size can thus be
   --  used to determine the space required for compiling the given regular
   --  expression.
   --
   --  This function raises Storage_Error if Matcher is too small to hold
   --  the resulting code (i.e. Matcher.Size has too small a value).
   --
   --  Expression_Error is raised if the string Expression does not contain
   --  a valid regular expression.
   --
   --  Flags is the default value to use to set properties for Expression (case
   --  sensitivity,...).

   procedure Compile
     (Matcher    : out Pattern_Matcher;
      Expression : String;
      Flags      : Regexp_Flags := No_Flags);
   --  Same procedure as above, expect it does not return the final
   --  program size, and Matcher.Size cannot be Auto_Size.

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

   --  The Match subprograms are given a regular expression in string
   --  form, and perform the corresponding match. The following parameters
   --  are present in all forms of the Match call.

   --    Expression contains the regular expression to be matched as a string

   --    Data contains the string to be matched

   --    Data_First is the lower bound for the match, i.e. Data (Data_First)
   --    will be the first character to be examined. If Data_First is set to
   --    the special value of -1 (the default), then the first character to
   --    be examined is Data (Data_First). However, the regular expression
   --    character ^ (start of string) still refers to the first character
   --    of the full string (Data (Data'First)), which is why there is a
   --    separate mechanism for specifying Data_First.

   --    Data_Last is the upper bound for the match, i.e. Data (Data_Last)
   --    will be the last character to be examined. If Data_Last is set to
   --    the special value of Positive'Last (the default), then the last
   --    character to be examined is Data (Data_Last). However, the regular
   --    expression character $ (end of string) still refers to the last
   --    character of the full string (Data (Data'Last)), which is why there
   --    is a separate mechanism for specifying Data_Last.

   --    Note: the use of Data_First and Data_Last is not equivalent to
   --    simply passing a slice as Expression because of the handling of
   --    regular expression characters ^ and $.

   --    Size is the size allocated for the compiled byte code. Normally
   --    this is defaulted to Auto_Size which means that the appropriate
   --    size is allocated automatically. It is possible to specify an
   --    explicit size, which must be sufficiently large. This slightly
   --    increases the efficiency by avoiding the extra step of computing
   --    the appropriate size.

   --  The following exceptions can be raised in calls to Match
   --
   --    Storage_Error is raised if a non-zero value is given for Size
   --    and it is too small to hold the compiled byte code.
   --
   --    Expression_Error is raised if the given expression is not a legal
   --    regular expression.

   procedure Match
     (Expression : String;
      Data       : String;
      Matches    : out Match_Array;
      Size       : Program_Size := Auto_Size;
      Data_First : Integer      := -1;
      Data_Last  : Positive     := Positive'Last);
   --  This version returns the result of the match stored in Match_Array;
   --  see comments under Match_Array above for details.

   function Match
     (Expression : String;
      Data       : String;
      Size       : Program_Size := Auto_Size;
      Data_First : Integer      := -1;
      Data_Last  : Positive     := Positive'Last) return Natural;
   --  This version returns the position where Data matches, or if there is
   --  no match, then the value Data'First - 1.

   function Match
     (Expression : String;
      Data       : String;
      Size       : Program_Size := Auto_Size;
      Data_First : Integer      := -1;
      Data_Last  : Positive     := Positive'Last) return Boolean;
   --  This version returns True if the match succeeds, False otherwise

   ------------------------------------------------
   -- Matching a Pre-Compiled Regular Expression --
   ------------------------------------------------

   --  The following functions are significantly faster if you need to reuse
   --  the same regular expression multiple times, since you only have to
   --  compile it once. For these functions you must first compile the
   --  expression with a call to Compile as previously described.

   --  The parameters Data, Data_First and Data_Last are as described
   --  in the previous section.

   function  Match
     (Self       : Pattern_Matcher;
      Data       : String;
      Data_First : Integer  := -1;
      Data_Last  : Positive := Positive'Last) return Natural;
   --  Match Data using the given pattern matcher. Returns the position
   --  where Data matches, or (Data'First - 1) if there is no match.

   function  Match
     (Self       : Pattern_Matcher;
      Data       : String;
      Data_First : Integer  := -1;
      Data_Last  : Positive := Positive'Last) return Boolean;
   --  Return True if Data matches using the given pattern matcher

   pragma Inline (Match);
   --  All except the last one below

   procedure Match
     (Self       : Pattern_Matcher;
      Data       : String;
      Matches    : out Match_Array;
      Data_First : Integer  := -1;
      Data_Last  : Positive := Positive'Last);
   --  Match Data using the given pattern matcher and store result in Matches;
   --  see comments under Match_Array above for details.

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

   --  The "internal use only" fields in regexp are present to pass info from
   --  compile to execute that permits the execute phase to run lots faster on
   --  simple cases. They are:

   --     First              character that must begin a match or ASCII.Nul
   --     Anchored           true iff match must start at beginning of line
   --     Must_Have          pointer to string that match must include or null
   --     Must_Have_Length   length of Must_Have string

   --  First and Anchored permit very fast decisions on suitable starting
   --  points for a match, cutting down the work a lot. Must_Have permits fast
   --  rejection of lines that cannot possibly match.

   --  The Must_Have tests are costly enough that Optimize supplies a Must_Have
   --  only if the r.e. contains something potentially expensive (at present,
   --  the only such thing detected is * or at the start of the r.e., which can
   --  involve a lot of backup). The length is supplied because the test in
   --  Execute needs it and Optimize is computing it anyway.

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

end System.Regpat;

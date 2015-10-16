------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                G N A T . S P I T B O L . P A T T E R N S                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 1997-2015, AdaCore                     --
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

--  SPITBOL-like pattern construction and matching

--  This child package of GNAT.SPITBOL provides a complete implementation
--  of the SPITBOL-like pattern construction and matching operations. This
--  package is based on Macro-SPITBOL created by Robert Dewar.

------------------------------------------------------------
-- Summary of Pattern Matching Packages in GNAT Hierarchy --
------------------------------------------------------------

--  There are three related packages that perform pattern matching functions.
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

with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Text_IO;      use Ada.Text_IO;

package GNAT.Spitbol.Patterns is
   pragma Elaborate_Body;

   -------------------------------
   -- Pattern Matching Tutorial --
   -------------------------------

   --  A pattern matching operation (a call to one of the Match subprograms)
   --  takes a subject string and a pattern, and optionally a replacement
   --  string. The replacement string option is only allowed if the subject
   --  is a variable.

   --  The pattern is matched against the subject string, and either the
   --  match fails, or it succeeds matching a contiguous substring. If a
   --  replacement string is specified, then the subject string is modified
   --  by replacing the matched substring with the given replacement.

   --  Concatenation and Alternation
   --  =============================

   --    A pattern consists of a series of pattern elements. The pattern is
   --    built up using either the concatenation operator:

   --       A & B

   --    which means match A followed immediately by matching B, or the
   --    alternation operator:

   --       A or B

   --    which means first attempt to match A, and then if that does not
   --    succeed, match B.

   --    There is full backtracking, which means that if a given pattern
   --    element fails to match, then previous alternatives are matched.
   --    For example if we have the pattern:

   --      (A or B) & (C or D) & (E or F)

   --    First we attempt to match A, if that succeeds, then we go on to try
   --    to match C, and if that succeeds, we go on to try to match E. If E
   --    fails, then we try F. If F fails, then we go back and try matching
   --    D instead of C. Let's make this explicit using a specific example,
   --    and introducing the simplest kind of pattern element, which is a
   --    literal string. The meaning of this pattern element is simply to
   --    match the characters that correspond to the string characters. Now
   --    let's rewrite the above pattern form with specific string literals
   --    as the pattern elements:

   --      ("ABC" or "AB") & ("DEF" or "CDE") & ("GH" or "IJ")

   --    The following strings will be attempted in sequence:

   --       ABC . DEF . GH
   --       ABC . DEF . IJ
   --       ABC . CDE . GH
   --       ABC . CDE . IJ
   --       AB . DEF . GH
   --       AB . DEF . IJ
   --       AB . CDE . GH
   --       AB . CDE . IJ

   --    Here we use the dot simply to separate the pieces of the string
   --    matched by the three separate elements.

   --  Moving the Start Point
   --  ======================

   --    A pattern is not required to match starting at the first character
   --    of the string, and is not required to match to the end of the string.
   --    The first attempt does indeed attempt to match starting at the first
   --    character of the string, trying all the possible alternatives. But
   --    if all alternatives fail, then the starting point of the match is
   --    moved one character, and all possible alternatives are attempted at
   --    the new anchor point.

   --    The entire match fails only when every possible starting point has
   --    been attempted. As an example, suppose that we had the subject
   --    string

   --      "ABABCDEIJKL"

   --    matched using the pattern in the previous example:

   --      ("ABC" or "AB") & ("DEF" or "CDE") & ("GH" or "IJ")

   --    would succeed, after two anchor point moves:

   --      "ABABCDEIJKL"
   --         ^^^^^^^
   --         matched
   --         section

   --    This mode of pattern matching is called the unanchored mode. It is
   --    also possible to put the pattern matcher into anchored mode by
   --    setting the global variable Anchored_Mode to True. This will cause
   --    all subsequent matches to be performed in anchored mode, where the
   --    match is required to start at the first character.

   --    We will also see later how the effect of an anchored match can be
   --    obtained for a single specified anchor point if this is desired.

   --  Other Pattern Elements
   --  ======================

   --    In addition to strings (or single characters), there are many special
   --    pattern elements that correspond to special predefined alternations:

   --      Arb       Matches any string. First it matches the null string, and
   --                then on a subsequent failure, matches one character, and
   --                then two characters, and so on. It only fails if the
   --                entire remaining string is matched.

   --      Bal       Matches a non-empty string that is parentheses balanced
   --                with respect to ordinary () characters. Examples of
   --                balanced strings are "ABC", "A((B)C)", and "A(B)C(D)E".
   --                Bal matches the shortest possible balanced string on the
   --                first attempt, and if there is a subsequent failure,
   --                attempts to extend the string.

   --      Cancel    Immediately aborts the entire pattern match, signalling
   --                failure. This is a specialized pattern element, which is
   --                useful in conjunction with some of the special pattern
   --                elements that have side effects.

   --      Fail      The null alternation. Matches no possible strings, so it
   --                always signals failure. This is a specialized pattern
   --                element, which is useful in conjunction with some of the
   --                special pattern elements that have side effects.

   --      Fence     Matches the null string at first, and then if a failure
   --                causes alternatives to be sought, aborts the match (like
   --                a Cancel). Note that using Fence at the start of a pattern
   --                has the same effect as matching in anchored mode.

   --      Rest      Matches from the current point to the last character in
   --                the string. This is a specialized pattern element, which
   --                is useful in conjunction with some of the special pattern
   --                elements that have side effects.

   --      Succeed   Repeatedly matches the null string (it is equivalent to
   --                the alternation ("" or "" or "" ....). This is a special
   --                pattern element, which is useful in conjunction with some
   --                of the special pattern elements that have side effects.

   --  Pattern Construction Functions
   --  ==============================

   --    The following functions construct additional pattern elements

   --      Any(S)    Where S is a string, matches a single character that is
   --                any one of the characters in S. Fails if the current
   --                character is not one of the given set of characters.

   --      Arbno(P)  Where P is any pattern, matches any number of instances
   --                of the pattern, starting with zero occurrences. It is
   --                thus equivalent to ("" or (P & ("" or (P & ("" ....)))).
   --                The pattern P may contain any number of pattern elements
   --                including the use of alternation and concatenation.

   --      Break(S)  Where S is a string, matches a string of zero or more
   --                characters up to but not including a break character
   --                that is one of the characters given in the string S.
   --                Can match the null string, but cannot match the last
   --                character in the string, since a break character is
   --                required to be present.

   --      BreakX(S) Where S is a string, behaves exactly like Break(S) when
   --                it first matches, but if a string is successfully matched,
   --                then a subsequent failure causes an attempt to extend the
   --                matched string.

   --      Fence(P)  Where P is a pattern, attempts to match the pattern P
   --                including trying all possible alternatives of P. If none
   --                of these alternatives succeeds, then the Fence pattern
   --                fails. If one alternative succeeds, then the pattern
   --                match proceeds, but on a subsequent failure, no attempt
   --                is made to search for alternative matches of P. The
   --                pattern P may contain any number of pattern elements
   --                including the use of alternation and concatenation.

   --      Len(N)    Where N is a natural number, matches the given number of
   --                characters. For example, Len(10) matches any string that
   --                is exactly ten characters long.

   --      NotAny(S) Where S is a string, matches a single character that is
   --                not one of the characters of S. Fails if the current
   --                character is one of the given set of characters.

   --      NSpan(S)  Where S is a string, matches a string of zero or more
   --                characters that is among the characters given in the
   --                string. Always matches the longest possible such string.
   --                Always succeeds, since it can match the null string.

   --      Pos(N)    Where N is a natural number, matches the null string
   --                if exactly N characters have been matched so far, and
   --                otherwise fails.

   --      Rpos(N)   Where N is a natural number, matches the null string
   --                if exactly N characters remain to be matched, and
   --                otherwise fails.

   --      Rtab(N)   Where N is a natural number, matches characters from
   --                the current position until exactly N characters remain
   --                to be matched in the string. Fails if fewer than N
   --                unmatched characters remain in the string.

   --      Tab(N)    Where N is a natural number, matches characters from
   --                the current position until exactly N characters have
   --                been matched in all. Fails if more than N characters
   --                have already been matched.

   --      Span(S)   Where S is a string, matches a string of one or more
   --                characters that is among the characters given in the
   --                string. Always matches the longest possible such string.
   --                Fails if the current character is not one of the given
   --                set of characters.

   --  Recursive Pattern Matching
   --  ==========================

   --    The plus operator (+P) where P is a pattern variable, creates
   --    a recursive pattern that will, at pattern matching time, follow
   --    the pointer to obtain the referenced pattern, and then match this
   --    pattern. This may be used to construct recursive patterns. Consider
   --    for example:

   --       P := ("A" or ("B" & (+P)))

   --    On the first attempt, this pattern attempts to match the string "A".
   --    If this fails, then the alternative matches a "B", followed by an
   --    attempt to match P again. This second attempt first attempts to
   --    match "A", and so on. The result is a pattern that will match a
   --    string of B's followed by a single A.

   --    This particular example could simply be written as NSpan('B') & 'A',
   --    but the use of recursive patterns in the general case can construct
   --    complex patterns which could not otherwise be built.

   --  Pattern Assignment Operations
   --  =============================

   --    In addition to the overall result of a pattern match, which indicates
   --    success or failure, it is often useful to be able to keep track of
   --    the pieces of the subject string that are matched by individual
   --    pattern elements, or subsections of the pattern.

   --    The pattern assignment operators allow this capability. The first
   --    form is the immediate assignment:

   --       P * S

   --    Here P is an arbitrary pattern, and S is a variable of type VString
   --    that will be set to the substring matched by P. This assignment
   --    happens during pattern matching, so if P matches more than once,
   --    then the assignment happens more than once.

   --    The deferred assignment operation:

   --      P ** S

   --    avoids these multiple assignments by deferring the assignment to the
   --    end of the match. If the entire match is successful, and if the
   --    pattern P was part of the successful match, then at the end of the
   --    matching operation the assignment to S of the string matching P is
   --    performed.

   --    The cursor assignment operation:

   --      Setcur(N'Access)

   --    assigns the current cursor position to the natural variable N. The
   --    cursor position is defined as the count of characters that have been
   --    matched so far (including any start point moves).

   --    Finally the operations * and ** may be used with values of type
   --    Text_IO.File_Access. The effect is to do a Put_Line operation of
   --    the matched substring. These are particularly useful in debugging
   --    pattern matches.

   --  Deferred Matching
   --  =================

   --    The pattern construction functions (such as Len and Any) all permit
   --    the use of pointers to natural or string values, or functions that
   --    return natural or string values. These forms cause the actual value
   --    to be obtained at pattern matching time. This allows interesting
   --    possibilities for constructing dynamic patterns as illustrated in
   --    the examples section.

   --    In addition the (+S) operator may be used where S is a pointer to
   --    string or function returning string, with a similar deferred effect.

   --    A special use of deferred matching is the construction of predicate
   --    functions. The element (+P) where P is an access to a function that
   --    returns a Boolean value, causes the function to be called at the
   --    time the element is matched. If the function returns True, then the
   --    null string is matched, if the function returns False, then failure
   --    is signalled and previous alternatives are sought.

   --  Deferred Replacement
   --  ====================

   --    The simple model given for pattern replacement (where the matched
   --    substring is replaced by the string given as the third argument to
   --    Match) works fine in simple cases, but this approach does not work
   --    in the case where the expression used as the replacement string is
   --    dependent on values set by the match.

   --    For example, suppose we want to find an instance of a parenthesized
   --    character, and replace the parentheses with square brackets. At first
   --    glance it would seem that:

   --      Match (Subject, '(' & Len (1) * Char & ')', '[' & Char & ']');

   --    would do the trick, but that does not work, because the third
   --    argument to Match gets evaluated too early, before the call to
   --    Match, and before the pattern match has had a chance to set Char.

   --    To solve this problem we provide the deferred replacement capability.
   --    With this approach, which of course is only needed if the pattern
   --    involved has side effects, is to do the match in two stages. The
   --    call to Match sets a pattern result in a variable of the private
   --    type Match_Result, and then a subsequent Replace operation uses
   --    this Match_Result object to perform the required replacement.

   --    Using this approach, we can now write the above operation properly
   --    in a manner that will work:

   --      M : Match_Result;
   --      ...
   --      Match (Subject, '(' & Len (1) * Char & ')', M);
   --      Replace (M, '[' & Char & ']');

   --    As with other Match cases, there is a function and procedure form
   --    of this match call. A call to Replace after a failed match has no
   --    effect. Note that Subject should not be modified between the calls.

   --  Examples of Pattern Matching
   --  ============================

   --    First a simple example of the use of pattern replacement to remove
   --    a line number from the start of a string. We assume that the line
   --    number has the form of a string of decimal digits followed by a
   --    period, followed by one or more spaces.

   --       Digs : constant Pattern := Span("0123456789");

   --       Lnum : constant Pattern := Pos(0) & Digs & '.' & Span(' ');

   --    Now to use this pattern we simply do a match with a replacement:

   --       Match (Line, Lnum, "");

   --    which replaces the line number by the null string. Note that it is
   --    also possible to use an Ada.Strings.Maps.Character_Set value as an
   --    argument to Span and similar functions, and in particular all the
   --    useful constants 'in Ada.Strings.Maps.Constants are available. This
   --    means that we could define Digs as:

   --       Digs : constant Pattern := Span(Decimal_Digit_Set);

   --    The style we use here, of defining constant patterns and then using
   --    them is typical. It is possible to build up patterns dynamically,
   --    but it is usually more efficient to build them in pieces in advance
   --    using constant declarations. Note in particular that although it is
   --    possible to construct a pattern directly as an argument for the
   --    Match routine, it is much more efficient to preconstruct the pattern
   --    as we did in this example.

   --    Now let's look at the use of pattern assignment to break a
   --    string into sections. Suppose that the input string has two
   --    unsigned decimal integers, separated by spaces or a comma,
   --    with spaces allowed anywhere. Then we can isolate the two
   --    numbers with the following pattern:

   --       Num1, Num2 : aliased VString;

   --       B : constant Pattern := NSpan(' ');

   --       N : constant Pattern := Span("0123456789");

   --       T : constant Pattern :=
   --             NSpan(' ') & N * Num1 & Span(" ,") & N * Num2;

   --    The match operation Match (" 124, 257  ", T) would assign the
   --    string 124 to Num1 and the string 257 to Num2.

   --    Now let's see how more complex elements can be built from the
   --    set of primitive elements. The following pattern matches strings
   --    that have the syntax of Ada 95 based literals:

   --       Digs  : constant Pattern := Span(Decimal_Digit_Set);
   --       UDigs : constant Pattern := Digs & Arbno('_' & Digs);

   --       Edig  : constant Pattern := Span(Hexadecimal_Digit_Set);
   --       UEdig : constant Pattern := Edig & Arbno('_' & Edig);

   --       Bnum  : constant Pattern := Udigs & '#' & UEdig & '#';

   --    A match against Bnum will now match the desired strings, e.g.
   --    it will match 16#123_abc#, but not a#b#. However, this pattern
   --    is not quite complete, since it does not allow colons to replace
   --    the pound signs. The following is more complete:

   --       Bchar : constant Pattern := Any("#:");
   --       Bnum  : constant Pattern := Udigs & Bchar & UEdig & Bchar;

   --    but that is still not quite right, since it allows # and : to be
   --    mixed, and they are supposed to be used consistently. We solve
   --    this by using a deferred match.

   --       Temp  : aliased VString;

   --       Bnum  : constant Pattern :=
   --                 Udigs & Bchar * Temp & UEdig & (+Temp)

   --    Here the first instance of the base character is stored in Temp, and
   --    then later in the pattern we rematch the value that was assigned.

   --    For an example of a recursive pattern, let's define a pattern
   --    that is like the built in Bal, but the string matched is balanced
   --    with respect to square brackets or curly brackets.

   --    The language for such strings might be defined in extended BNF as

   --      ELEMENT ::= <any character other than [] or {}>
   --                  | '[' BALANCED_STRING ']'
   --                  | '{' BALANCED_STRING '}'

   --      BALANCED_STRING ::= ELEMENT {ELEMENT}

   --    Here we use {} to indicate zero or more occurrences of a term, as
   --    is common practice in extended BNF. Now we can translate the above
   --    BNF into recursive patterns as follows:

   --      Element, Balanced_String : aliased Pattern;
   --      .
   --      .
   --      .
   --      Element := NotAny ("[]{}")
   --                   or
   --                 ('[' & (+Balanced_String) & ']')
   --                   or
   --                 ('{' & (+Balanced_String) & '}');

   --      Balanced_String := Element & Arbno (Element);

   --    Note the important use of + here to refer to a pattern not yet
   --    defined. Note also that we use assignments precisely because we
   --    cannot refer to as yet undeclared variables in initializations.

   --    Now that this pattern is constructed, we can use it as though it
   --    were a new primitive pattern element, and for example, the match:

   --      Match ("xy[ab{cd}]", Balanced_String * Current_Output & Fail);

   --    will generate the output:

   --       x
   --       xy
   --       xy[ab{cd}]
   --       y
   --       y[ab{cd}]
   --       [ab{cd}]
   --       a
   --       ab
   --       ab{cd}
   --       b
   --       b{cd}
   --       {cd}
   --       c
   --       cd
   --       d

   --    Note that the function of the fail here is simply to force the
   --    pattern Balanced_String to match all possible alternatives. Studying
   --    the operation of this pattern in detail is highly instructive.

   --    Finally we give a rather elaborate example of the use of deferred
   --    matching. The following declarations build up a pattern which will
   --    find the longest string of decimal digits in the subject string.

   --       Max, Cur : VString;
   --       Loc      : Natural;

   --       function GtS return Boolean is
   --       begin
   --          return Length (Cur) > Length (Max);
   --       end GtS;

   --       Digit : constant Character_Set := Decimal_Digit_Set;

   --       Digs  : constant Pattern := Span(Digit);

   --       Find : constant Pattern :=
   --         "" * Max & Fence            & -- initialize Max to null
   --         BreakX (Digit)              & -- scan looking for digits
   --         ((Span(Digit) * Cur         & -- assign next string to Cur
   --          (+GtS'Unrestricted_Access) & -- check size(Cur) > Size(Max)
   --          Setcur(Loc'Access))          -- if so, save location
   --                   * Max)            & -- and assign to Max
   --         Fail;                         -- seek all alternatives

   --    As we see from the comments here, complex patterns like this take
   --    on aspects of sequential programs. In fact they are sequential
   --    programs with general backtracking. In this pattern, we first use
   --    a pattern assignment that matches null and assigns it to Max, so
   --    that it is initialized for the new match. Now BreakX scans to the
   --    next digit. Arb would do here, but BreakX will be more efficient.
   --    Once we have found a digit, we scan out the longest string of
   --    digits with Span, and assign it to Cur. The deferred call to GtS
   --    tests if the string we assigned to Cur is the longest so far. If
   --    not, then failure is signalled, and we seek alternatives (this
   --    means that BreakX will extend and look for the next digit string).
   --    If the call to GtS succeeds then the matched string is assigned
   --    as the largest string so far into Max and its location is saved
   --    in Loc. Finally Fail forces the match to fail and seek alternatives,
   --    so that the entire string is searched.

   --    If the pattern Find is matched against a string, the variable Max
   --    at the end of the pattern will have the longest string of digits,
   --    and Loc will be the starting character location of the string. For
   --    example, Match("ab123cd4657ef23", Find) will assign "4657" to Max
   --    and 11 to Loc (indicating that the string ends with the eleventh
   --    character of the string).

   --    Note: the use of Unrestricted_Access to reference GtS will not
   --    be needed if GtS is defined at the outer level, but definitely
   --    will be necessary if GtS is a nested function (in which case of
   --    course the scope of the pattern Find will be restricted to this
   --    nested scope, and this cannot be checked, i.e. use of the pattern
   --    outside this scope is erroneous). Generally it is a good idea to
   --    define patterns and the functions they call at the outer level
   --    where possible, to avoid such problems.

   --  Correspondence with Pattern Matching in SPITBOL
   --  ===============================================

   --    Generally the Ada syntax and names correspond closely to SPITBOL
   --    syntax for pattern matching construction.

   --      The basic pattern construction operators are renamed as follows:

   --          Spitbol     Ada

   --          (space)      &
   --             |         or
   --             $         *
   --             .         **

   --      The Ada operators were chosen so that the relative precedences of
   --      these operators corresponds to that of the Spitbol operators, but
   --      as always, the use of parentheses is advisable to clarify.

   --    The pattern construction operators all have similar names except for

   --          Spitbol      Ada

   --          Abort        Cancel
   --          Rem          Rest

   --    where we have clashes with Ada reserved names

   --    Ada requires the use of 'Access to refer to functions used in the
   --    pattern match, and often the use of 'Unrestricted_Access may be
   --    necessary to get around the scope restrictions if the functions
   --    are not declared at the outer level.

   --    The actual pattern matching syntax is modified in Ada as follows:

   --          Spitbol      Ada

   --          X Y          Match (X, Y);
   --          X Y = Z      Match (X, Y, Z);

   --    and pattern failure is indicated by returning a Boolean result from
   --    the Match function (True for success, False for failure).

   -----------------------
   -- Type Declarations --
   -----------------------

   type Pattern is private;
   --  Type representing a pattern. This package provides a complete set of
   --  operations for constructing patterns that can be used in the pattern
   --  matching operations provided.

   type Boolean_Func is access function return Boolean;
   --  General Boolean function type. When this type is used as a formal
   --  parameter type in this package, it indicates a deferred predicate
   --  pattern. The function will be called when the pattern element is
   --  matched and failure signalled if False is returned.

   type Natural_Func is access function return Natural;
   --  General Natural function type. When this type is used as a formal
   --  parameter type in this package, it indicates a deferred pattern.
   --  The function will be called when the pattern element is matched
   --  to obtain the currently referenced Natural value.

   type VString_Func is access function return VString;
   --  General VString function type. When this type is used as a formal
   --  parameter type in this package, it indicates a deferred pattern.
   --  The function will be called when the pattern element is matched
   --  to obtain the currently referenced string value.

   subtype PString is String;
   --  This subtype is used in the remainder of the package to indicate a
   --  formal parameter that is converted to its corresponding pattern,
   --  i.e. a pattern that matches the characters of the string.

   subtype PChar is Character;
   --  Similarly, this subtype is used in the remainder of the package to
   --  indicate a formal parameter that is converted to its corresponding
   --  pattern, i.e. a pattern that matches this one character.

   subtype VString_Var is VString;
   subtype Pattern_Var is Pattern;
   --  These synonyms are used as formal parameter types to a function where,
   --  if the language allowed, we would use in out parameters, but we are
   --  not allowed to have in out parameters for functions. Instead we pass
   --  actuals which must be variables, and with a bit of trickery in the
   --  body, manage to interpret them properly as though they were indeed
   --  in out parameters.

   pragma Warnings (Off, VString_Var);
   pragma Warnings (Off, Pattern_Var);
   --  We turn off warnings for these two types so that when variables are used
   --  as arguments in this context, warnings about them not being assigned in
   --  the source program will be suppressed.

   --------------------------------
   -- Basic Pattern Construction --
   --------------------------------

   function "&"  (L : Pattern; R : Pattern) return Pattern;
   function "&"  (L : PString; R : Pattern) return Pattern;
   function "&"  (L : Pattern; R : PString) return Pattern;
   function "&"  (L : PChar;   R : Pattern) return Pattern;
   function "&"  (L : Pattern; R : PChar)   return Pattern;

   --  Pattern concatenation. Matches L followed by R

   function "or" (L : Pattern; R : Pattern) return Pattern;
   function "or" (L : PString; R : Pattern) return Pattern;
   function "or" (L : Pattern; R : PString) return Pattern;
   function "or" (L : PString; R : PString) return Pattern;
   function "or" (L : PChar;   R : Pattern) return Pattern;
   function "or" (L : Pattern; R : PChar)   return Pattern;
   function "or" (L : PChar;   R : PChar)   return Pattern;
   function "or" (L : PString; R : PChar)   return Pattern;
   function "or" (L : PChar;   R : PString) return Pattern;
   --  Pattern alternation. Creates a pattern that will first try to match
   --  L and then on a subsequent failure, attempts to match R instead.

   ----------------------------------
   -- Pattern Assignment Functions --
   ----------------------------------

   function "*" (P : Pattern; Var : VString_Var)  return Pattern;
   function "*" (P : PString; Var : VString_Var)  return Pattern;
   function "*" (P : PChar;   Var : VString_Var)  return Pattern;
   --  Matches P, and if the match succeeds, assigns the matched substring
   --  to the given VString variable Var. This assignment happens as soon as
   --  the substring is matched, and if the pattern P1 is matched more than
   --  once during the course of the match, then the assignment will occur
   --  more than once.

   function "**" (P : Pattern; Var : VString_Var) return Pattern;
   function "**" (P : PString; Var : VString_Var) return Pattern;
   function "**" (P : PChar;   Var : VString_Var) return Pattern;
   --  Like "*" above, except that the assignment happens at most once
   --  after the entire match is completed successfully. If the match
   --  fails, then no assignment takes place.

   ----------------------------------
   -- Deferred Matching Operations --
   ----------------------------------

   function "+" (Str : VString_Var)  return Pattern;
   --  Here Str must be a VString variable. This function constructs a
   --  pattern which at pattern matching time will access the current
   --  value of this variable, and match against these characters.

   function "+" (Str : VString_Func) return Pattern;
   --  Constructs a pattern which at pattern matching time calls the given
   --  function, and then matches against the string or character value
   --  that is returned by the call.

   function "+" (P : Pattern_Var)    return Pattern;
   --  Here P must be a Pattern variable. This function constructs a
   --  pattern which at pattern matching time will access the current
   --  value of this variable, and match against the pattern value.

   function "+" (P : Boolean_Func)   return Pattern;
   --  Constructs a predicate pattern function that at pattern matching time
   --  calls the given function. If True is returned, then the pattern matches.
   --  If False is returned, then failure is signalled.

   --------------------------------
   -- Pattern Building Functions --
   --------------------------------

   function Arb                                             return Pattern;
   --  Constructs a pattern that will match any string. On the first attempt,
   --  the pattern matches a null string, then on each successive failure, it
   --  matches one more character, and only fails if matching the entire rest
   --  of the string.

   function Arbno  (P : Pattern)                            return Pattern;
   function Arbno  (P : PString)                            return Pattern;
   function Arbno  (P : PChar)                              return Pattern;
   --  Pattern repetition. First matches null, then on a subsequent failure
   --  attempts to match an additional instance of the given pattern.
   --  Equivalent to (but more efficient than) P & ("" or (P & ("" or ...

   function Any    (Str : String)                           return Pattern;
   function Any    (Str : VString)                          return Pattern;
   function Any    (Str : Character)                        return Pattern;
   function Any    (Str : Character_Set)                    return Pattern;
   function Any    (Str : not null access VString)          return Pattern;
   function Any    (Str : VString_Func)                     return Pattern;
   --  Constructs a pattern that matches a single character that is one of
   --  the characters in the given argument. The pattern fails if the current
   --  character is not in Str.

   function Bal                                             return Pattern;
   --  Constructs a pattern that will match any non-empty string that is
   --  parentheses balanced with respect to the normal parentheses characters.
   --  Attempts to extend the string if a subsequent failure occurs.

   function Break  (Str : String)                           return Pattern;
   function Break  (Str : VString)                          return Pattern;
   function Break  (Str : Character)                        return Pattern;
   function Break  (Str : Character_Set)                    return Pattern;
   function Break  (Str : not null access VString)          return Pattern;
   function Break  (Str : VString_Func)                     return Pattern;
   --  Constructs a pattern that matches a (possibly null) string which
   --  is immediately followed by a character in the given argument. This
   --  character is not part of the matched string. The pattern fails if
   --  the remaining characters to be matched do not include any of the
   --  characters in Str.

   function BreakX (Str : String)                           return Pattern;
   function BreakX (Str : VString)                          return Pattern;
   function BreakX (Str : Character)                        return Pattern;
   function BreakX (Str : Character_Set)                    return Pattern;
   function BreakX (Str : not null access VString)          return Pattern;
   function BreakX (Str : VString_Func)                     return Pattern;
   --  Like Break, but the pattern attempts to extend on a failure to find
   --  the next occurrence of a character in Str, and only fails when the
   --  last such instance causes a failure.

   function Cancel                                          return Pattern;
   --  Constructs a pattern that immediately aborts the entire match

   function Fail                                            return Pattern;
   --  Constructs a pattern that always fails

   function Fence                                           return Pattern;
   --  Constructs a pattern that matches null on the first attempt, and then
   --  causes the entire match to be aborted if a subsequent failure occurs.

   function Fence  (P : Pattern)                            return Pattern;
   --  Constructs a pattern that first matches P. If P fails, then the
   --  constructed pattern fails. If P succeeds, then the match proceeds,
   --  but if subsequent failure occurs, alternatives in P are not sought.
   --  The idea of Fence is that each time the pattern is matched, just
   --  one attempt is made to match P, without trying alternatives.

   function Len    (Count : Natural)                        return Pattern;
   function Len    (Count : not null access Natural)        return Pattern;
   function Len    (Count : Natural_Func)                   return Pattern;
   --  Constructs a pattern that matches exactly the given number of
   --  characters. The pattern fails if fewer than this number of characters
   --  remain to be matched in the string.

   function NotAny (Str : String)                           return Pattern;
   function NotAny (Str : VString)                          return Pattern;
   function NotAny (Str : Character)                        return Pattern;
   function NotAny (Str : Character_Set)                    return Pattern;
   function NotAny (Str : not null access VString)          return Pattern;
   function NotAny (Str : VString_Func)                     return Pattern;
   --  Constructs a pattern that matches a single character that is not
   --  one of the characters in the given argument. The pattern Fails if
   --  the current character is in Str.

   function NSpan  (Str : String)                           return Pattern;
   function NSpan  (Str : VString)                          return Pattern;
   function NSpan  (Str : Character)                        return Pattern;
   function NSpan  (Str : Character_Set)                    return Pattern;
   function NSpan  (Str : not null access VString)          return Pattern;
   function NSpan  (Str : VString_Func)                     return Pattern;
   --  Constructs a pattern that matches the longest possible string
   --  consisting entirely of characters from the given argument. The
   --  string may be empty, so this pattern always succeeds.

   function Pos    (Count : Natural)                        return Pattern;
   function Pos    (Count : not null access Natural)        return Pattern;
   function Pos    (Count : Natural_Func)                   return Pattern;
   --  Constructs a pattern that matches the null string if exactly Count
   --  characters have already been matched, and otherwise fails.

   function Rest                                            return Pattern;
   --  Constructs a pattern that always succeeds, matching the remaining
   --  unmatched characters in the pattern.

   function Rpos   (Count : Natural)                        return Pattern;
   function Rpos   (Count : not null access Natural)        return Pattern;
   function Rpos   (Count : Natural_Func)                   return Pattern;
   --  Constructs a pattern that matches the null string if exactly Count
   --  characters remain to be matched in the string, and otherwise fails.

   function Rtab   (Count : Natural)                        return Pattern;
   function Rtab   (Count : not null access Natural)        return Pattern;
   function Rtab   (Count : Natural_Func)                   return Pattern;
   --  Constructs a pattern that matches from the current location until
   --  exactly Count characters remain to be matched in the string. The
   --  pattern fails if fewer than Count characters remain to be matched.

   function Setcur (Var : not null access Natural)          return Pattern;
   --  Constructs a pattern that matches the null string, and assigns the
   --  current cursor position in the string. This value is the number of
   --  characters matched so far. So it is zero at the start of the match.

   function Span   (Str : String)                           return Pattern;
   function Span   (Str : VString)                          return Pattern;
   function Span   (Str : Character)                        return Pattern;
   function Span   (Str : Character_Set)                    return Pattern;
   function Span   (Str : not null access VString)          return Pattern;
   function Span   (Str : VString_Func)                     return Pattern;
   --  Constructs a pattern that matches the longest possible string
   --  consisting entirely of characters from the given argument. The
   --  string cannot be empty, so the pattern fails if the current
   --  character is not one of the characters in Str.

   function Succeed                                         return Pattern;
   --  Constructs a pattern that succeeds matching null, both on the first
   --  attempt, and on any rematch attempt, i.e. it is equivalent to an
   --  infinite alternation of null strings.

   function Tab    (Count : Natural)                        return Pattern;
   function Tab    (Count : not null access Natural)        return Pattern;
   function Tab    (Count : Natural_Func)                   return Pattern;
   --  Constructs a pattern that from the current location until Count
   --  characters have been matched. The pattern fails if more than Count
   --  characters have already been matched.

   ---------------------------------
   -- Pattern Matching Operations --
   ---------------------------------

   --  The Match function performs an actual pattern matching operation.
   --  The versions with three parameters perform a match without modifying
   --  the subject string and return a Boolean result indicating if the
   --  match is successful or not. The Anchor parameter is set to True to
   --  obtain an anchored match in which the pattern is required to match
   --  the first character of the string. In an unanchored match, which is

   --  the default, successive attempts are made to match the given pattern
   --  at each character of the subject string until a match succeeds, or
   --  until all possibilities have failed.

   --  Note that pattern assignment functions in the pattern may generate
   --  side effects, so these functions are not necessarily pure.

   Anchored_Mode : Boolean := False;
   --  This global variable can be set True to cause all subsequent pattern
   --  matches to operate in anchored mode. In anchored mode, no attempt is
   --  made to move the anchor point, so that if the match succeeds it must
   --  succeed starting at the first character. Note that the effect of
   --  anchored mode may be achieved in individual pattern matches by using
   --  Fence or Pos(0) at the start of the pattern.

   Pattern_Stack_Overflow : exception;
   --  Exception raised if internal pattern matching stack overflows. This
   --  is typically the result of runaway pattern recursion. If there is a
   --  genuine case of stack overflow, then either the match must be broken
   --  down into simpler steps, or the stack limit must be reset.

   Stack_Size : constant Positive := 2000;
   --  Size used for internal pattern matching stack. Increase this size if
   --  complex patterns cause Pattern_Stack_Overflow to be raised.

   --  Simple match functions. The subject is matched against the pattern.
   --  Any immediate or deferred assignments or writes are executed, and
   --  the returned value indicates whether or not the match succeeded.

   function Match
     (Subject : VString;
      Pat     : Pattern) return Boolean;

   function Match
     (Subject : VString;
      Pat     : PString) return Boolean;

   function Match
     (Subject : String;
      Pat     : Pattern) return Boolean;

   function Match
     (Subject : String;
      Pat     : PString) return Boolean;

   --  Replacement functions. The subject is matched against the pattern.
   --  Any immediate or deferred assignments or writes are executed, and
   --  the returned value indicates whether or not the match succeeded.
   --  If the match succeeds, then the matched part of the subject string
   --  is replaced by the given Replace string.

   function Match
     (Subject : VString_Var;
      Pat     : Pattern;
      Replace : VString) return Boolean;

   function Match
     (Subject : VString_Var;
      Pat     : PString;
      Replace : VString) return Boolean;

   function Match
     (Subject : VString_Var;
      Pat     : Pattern;
      Replace : String) return Boolean;

   function Match
     (Subject : VString_Var;
      Pat     : PString;
      Replace : String) return Boolean;

   --  Simple match procedures. The subject is matched against the pattern.
   --  Any immediate or deferred assignments or writes are executed. No
   --  indication of success or failure is returned.

   procedure Match
     (Subject : VString;
      Pat     : Pattern);

   procedure Match
     (Subject : VString;
      Pat     : PString);

   procedure Match
     (Subject : String;
      Pat     : Pattern);

   procedure Match
     (Subject : String;
      Pat     : PString);

   --  Replacement procedures. The subject is matched against the pattern.
   --  Any immediate or deferred assignments or writes are executed. No
   --  indication of success or failure is returned. If the match succeeds,
   --  then the matched part of the subject string is replaced by the given
   --  Replace string.

   procedure Match
     (Subject : in out VString;
      Pat     : Pattern;
      Replace : VString);

   procedure Match
     (Subject : in out VString;
      Pat     : PString;
      Replace : VString);

   procedure Match
     (Subject : in out VString;
      Pat     : Pattern;
      Replace : String);

   procedure Match
     (Subject : in out VString;
      Pat     : PString;
      Replace : String);

   --  Deferred Replacement

   type Match_Result is private;
   --  Type used to record result of pattern match

   subtype Match_Result_Var is Match_Result;
   --  This synonyms is used as a formal parameter type to a function where,
   --  if the language allowed, we would use an in out parameter, but we are
   --  not allowed to have in out parameters for functions. Instead we pass
   --  actuals which must be variables, and with a bit of trickery in the
   --  body, manage to interpret them properly as though they were indeed
   --  in out parameters.

   function Match
     (Subject : VString_Var;
      Pat     : Pattern;
      Result  : Match_Result_Var) return Boolean;

   procedure Match
     (Subject : in out VString;
      Pat     : Pattern;
      Result  : out Match_Result);

   procedure Replace
     (Result  : in out Match_Result;
      Replace : VString);
   --  Given a previous call to Match which set Result, performs a pattern
   --  replacement if the match was successful. Has no effect if the match
   --  failed. This call should immediately follow the Match call.

   ------------------------
   -- Debugging Routines --
   ------------------------

   --  Debugging pattern matching operations can often be quite complex,
   --  since there is no obvious way to trace the progress of the match.
   --  The declarations in this section provide some debugging assistance.

   Debug_Mode : Boolean := False;
   --  This global variable can be set True to generate debugging on all
   --  subsequent calls to Match. The debugging output is a full trace of
   --  the actions of the pattern matcher, written to Standard_Output. The
   --  level of this information is intended to be comprehensible at the
   --  abstract level of this package declaration. However, note that the
   --  use of this switch often generates large amounts of output.

   function "*"  (P : Pattern; Fil : File_Access)           return Pattern;
   function "*"  (P : PString; Fil : File_Access)           return Pattern;
   function "*"  (P : PChar;   Fil : File_Access)           return Pattern;
   function "**" (P : Pattern; Fil : File_Access)           return Pattern;
   function "**" (P : PString; Fil : File_Access)           return Pattern;
   function "**" (P : PChar;   Fil : File_Access)           return Pattern;
   --  These are similar to the corresponding pattern assignment operations
   --  except that instead of setting the value of a variable, the matched
   --  substring is written to the appropriate file. This can be useful in
   --  following the progress of a match without generating the full amount
   --  of information obtained by setting Debug_Mode to True.

   Terminal : constant File_Access := Standard_Error;
   Output   : constant File_Access := Standard_Output;
   --  Two handy synonyms for use with the above pattern write operations

   --  Finally we have some routines that are useful for determining what
   --  patterns are in use, particularly if they are constructed dynamically.

   function Image (P : Pattern) return String;
   function Image (P : Pattern) return VString;
   --  This procedures yield strings that corresponds to the syntax needed
   --  to create the given pattern using the functions in this package. The
   --  form of this string is such that it could actually be compiled and
   --  evaluated to yield the required pattern except for references to
   --  variables and functions, which are output using one of the following
   --  forms:
   --
   --     access Natural     NP(16#...#)
   --     access Pattern     PP(16#...#)
   --     access VString     VP(16#...#)
   --
   --     Natural_Func       NF(16#...#)
   --     VString_Func       VF(16#...#)
   --
   --  where 16#...# is the hex representation of the integer address that
   --  corresponds to the given access value

   procedure Dump (P : Pattern);
   --  This procedure writes information about the pattern to Standard_Out.
   --  The format of this information is keyed to the internal data structures
   --  used to implement patterns. The information provided by Dump is thus
   --  more precise than that yielded by Image, but is also a bit more obscure
   --  (i.e. it cannot be interpreted solely in terms of this spec, you have
   --  to know something about the data structures).

   ------------------
   -- Private Part --
   ------------------

private
   type PE;
   --  Pattern element, a pattern is a complex structure of PE's. This type
   --  is defined and described in the body of this package.

   type PE_Ptr is access all PE;
   --  Pattern reference. PE's use PE_Ptr values to reference other PE's

   type Pattern is new Controlled with record
      Stk : Natural := 0;
      --  Maximum number of stack entries required for matching this
      --  pattern. See description of pattern history stack in body.

      P : PE_Ptr := null;
      --  Pointer to initial pattern element for pattern
   end record;

   pragma Finalize_Storage_Only (Pattern);

   procedure Adjust (Object : in out Pattern);
   --  Adjust routine used to copy pattern objects

   procedure Finalize (Object : in out Pattern);
   --  Finalization routine used to release storage allocated for a pattern

   type VString_Ptr is access all VString;

   type Match_Result is record
      Var : VString_Ptr;
      --  Pointer to subject string. Set to null if match failed

      Start : Natural := 1;
      --  Starting index position (1's origin) of matched section of
      --  subject string. Only valid if Var is non-null.

      Stop : Natural := 0;
      --  Ending index position (1's origin) of matched section of
      --  subject string. Only valid if Var is non-null.

   end record;

   pragma Volatile (Match_Result);
   --  This ensures that the Result parameter is passed by reference, so
   --  that we can play our games with the bogus Match_Result_Var parameter
   --  in the function case to treat it as though it were an in out parameter.

end GNAT.Spitbol.Patterns;

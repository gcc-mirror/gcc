------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              S T Y L E S W                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2020, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains the style switches used for setting style options.
--  The only clients of this package are the body of Style and the body of
--  Switches. All other style checking issues are handled using the public
--  interfaces in the spec of Style.

with Types; use Types;

package Stylesw is

   --------------------------
   -- Style Check Switches --
   --------------------------

   --  These flags are used to control the details of the style checking
   --  options. The default values shown here correspond to no style checking.

   --  If any of these values is set to a non-default value, then
   --  Opt.Style_Check is set True to activate calls to this package.

   --  The actual mechanism for setting these switches to other than default
   --  values is via the Set_Style_Check_Options procedure or through a call to
   --  Set_Default_Style_Check_Options. They should not be set directly in any
   --  other manner.

   Style_Check_Array_Attribute_Index : Boolean := False;
   --  This can be set True by using the -gnatyA switch. If it is True then
   --  index numbers for array attributes (like Length) are required to be
   --  absent for one-dimensional arrays and present for multi-dimensional
   --  array attribute references.

   Style_Check_Attribute_Casing : Boolean := False;
   --  This can be set True by using the -gnatya switch. If it is True, then
   --  attribute names (including keywords such as digits used as attribute
   --  names) must be in mixed case.

   Style_Check_Blanks_At_End : Boolean := False;
   --  This can be set True by using the -gnatyb switch. If it is True, then
   --  spaces at the end of lines are not permitted.

   Style_Check_Blank_Lines : Boolean := False;
   --  This can be set True by using the -gnatyu switch. If it is True, then
   --  multiple blank lines are not permitted, and there may not be a blank
   --  line at the end of the file.

   Style_Check_Boolean_And_Or : Boolean := False;
   --  This can be set True by using the -gnatyB switch. If it is True, then
   --  the use of AND THEN/OR ELSE rather than AND/OR is required except for
   --  the following cases:
   --
   --     a) Both operands are simple Boolean constants or variables
   --     b) Both operands are of a modular type
   --     c) Both operands are of an array type

   Style_Check_Comments : Boolean := False;
   --  This can be set True by using the -gnatyc switch. If it is True, then
   --  comments are style checked as follows:
   --
   --    All comments must be at the start of the line, or the first minus must
   --    be preceded by at least one space.
   --
   --    For a comment that is not at the start of a line, the only requirement
   --    is that a space follow the comment characters.
   --
   --    For a comment that is at the start of the line, one of the following
   --    conditions must hold:
   --
   --      The comment characters are the only non-blank characters on the line
   --
   --      The comment characters are followed by an exclamation point (the
   --      sequence --! is used by gnatprep for marking deleted lines).
   --
   --      The comment characters are followed by two space characters if
   --      Comment_Spacing = 2, else by one character if Comment_Spacing = 1.
   --
   --      The line consists entirely of minus signs
   --
   --      The comment characters are followed by a single space, and the last
   --      two characters on the line are also comment characters.
   --
   --  Note: the reason for the last two conditions is to allow "boxed"
   --  comments where only a single space separates the comment characters.

   Style_Check_Comments_Spacing : Nat range 1 .. 2;
   --  Spacing required for comments, valid only if Style_Check_Comments true.

   Style_Check_DOS_Line_Terminator : Boolean := False;
   --  This can be set true by using the -gnatyd switch. If it is True, then
   --  the line terminator must be a single LF, without an associated CR (e.g.
   --  DOS line terminator sequence CR/LF not allowed).

   Style_Check_Mixed_Case_Decls : Boolean := False;
   --  This can be set True by using the -gnatyD switch. If it is True, then
   --  declared identifiers must be in Mixed_Case.

   Style_Check_End_Labels : Boolean := False;
   --  This can be set True by using the -gnatye switch. If it is True, then
   --  optional END labels must always be present.

   Style_Check_Form_Feeds : Boolean := False;
   --  This can be set True by using the -gnatyf switch. If it is True, then
   --  form feeds and vertical tabs are not allowed in the source text.

   Style_Check_Horizontal_Tabs : Boolean := False;
   --  This can be set True by using the -gnatyh switch. If it is True, then
   --  horizontal tabs are not allowed in source text.

   Style_Check_If_Then_Layout : Boolean := False;
   --  This can be set True by using the -gnatyi switch. If it is True, then a
   --  THEN keyword must either appear on the same line as the IF, or on a line
   --  all on its own.
   --
   --  This permits one of two styles for IF-THEN layout. Either the IF and
   --  THEN keywords are on the same line, where the condition is short enough,
   --  or the conditions are continued over to the lines following the IF and
   --  the THEN stands on its own. For example:
   --
   --    if X > Y then
   --
   --    if X > Y
   --      and then Y < Z
   --    then
   --
   --    if X > Y and then Z > 0
   --    then
   --
   --  are allowed, but
   --
   --    if X > Y
   --      and then B > C then
   --
   --  is not allowed.

   Style_Check_Indentation : Column_Number range 0 .. 9 := 0;
   --  This can be set non-zero by using the -gnaty? (? a digit) switch. If
   --  it is non-zero it activates indentation checking with the indicated
   --  indentation value. A value of zero turns off checking. The requirement
   --  is that any new statement, line comment, declaration or keyword such
   --  as END, start on a column that is a multiple of the indentation value.

   Style_Check_Keyword_Casing : Boolean := False;
   --  This can be set True by using the -gnatyk switch. If it is True, then
   --  keywords are required to be in all lower case. This rule does not apply
   --  to keywords such as digits appearing as an attribute name.

   Style_Check_Layout : Boolean := False;
   --  This can be set True by using the -gnatyl switch. If it is True, it
   --  activates checks that constructs are indented as suggested by the
   --  examples in the RM syntax, e.g. that the ELSE keyword must line up
   --  with the IF keyword.

   Style_Check_Max_Line_Length : Boolean := False;
   --  This can be set True by using the -gnatym/M switches. If it is True, it
   --  activates checking for a maximum line length of Style_Max_Line_Length
   --  characters.

   Style_Check_Max_Nesting_Level : Boolean := False;
   --  This can be set True by using -gnatyLnnn with a value other than zero
   --  (a value of zero resets it to False). If True, it activates checking
   --  the maximum nesting level against Style_Max_Nesting_Level.

   Style_Check_Missing_Overriding : Boolean := False;
   --  This can be set True by using the -gnatyO switch. If it is True, then
   --  "overriding" is required in subprogram declarations and bodies where
   --  appropriate. Note that "not overriding" is never required.

   Style_Check_Mode_In : Boolean := False;
   --  This can be set True by using -gnatyI. If True, it activates checking
   --  that mode IN is not used on its own (since it is the default).

   Style_Check_Order_Subprograms : Boolean := False;
   --  This can be set True by using the -gnatyo switch. If it is True, then
   --  names of subprogram bodies must be in alphabetical order (not taking
   --  casing into account).

   Style_Check_Pragma_Casing : Boolean := False;
   --  This can be set True by using the -gnatyp switch. If it is True, then
   --  pragma names must use mixed case.

   Style_Check_References : Boolean := False;
   --  This can be set True by using the -gnatyr switch. If it is True, then
   --  all references to declared identifiers are checked. The requirement
   --  is that casing of the reference be the same as the casing of the
   --  corresponding declaration.

   Style_Check_Separate_Stmt_Lines : Boolean := False;
   --  This can be set True by using the -gnatyS switch. If it is TRUE,
   --  then for the case of keywords THEN (not preceded by AND) or ELSE (not
   --  preceded by OR) which introduce a conditionally executed statement
   --  sequence, there must be no tokens on the same line as the keyword, so
   --  that coverage testing can clearly identify execution of the statement
   --  sequence. A comment is permitted, as is THEN ABORT or a PRAGMA keyword
   --  after ELSE (a common style to specify the condition for the ELSE).

   Style_Check_Specs : Boolean := False;
   --  This can be set True by using the -gnatys switches. If it is True, then
   --  separate specs are required to be present for all procedures except
   --  parameterless library level procedures. The exception means that typical
   --  main programs do not require separate specs.

   Style_Check_Standard : Boolean := False;
   --  This can be set True by using the -gnatyn switch. If it is True, then
   --  any references to names in Standard have to be cased in a manner that
   --  is consistent with the Ada RM (usually Mixed case, as in Long_Integer)
   --  but there are some exceptions (e.g. NUL, ASCII).

   Style_Check_Tokens : Boolean := False;
   --  This can be set True by using the -gnatyt switch. If it is True, then
   --  the style check that requires canonical spacing between various
   --  punctuation tokens as follows:
   --
   --    ABS and NOT must be followed by a space
   --
   --    => must be surrounded by spaces
   --
   --    <> must be preceded by a space or left paren
   --
   --    Binary operators other than ** must be surrounded by spaces.
   --
   --    There is no restriction on the layout of the ** binary operator.
   --
   --    Colon must be surrounded by spaces
   --
   --    Colon-equal (assignment) must be surrounded by spaces
   --
   --    Comma must be the first non-blank character on the line, or be
   --    immediately preceded by a non-blank character, and must be followed
   --    by a blank.
   --
   --    A space must precede a left paren following a digit or letter, and a
   --    right paren must not be followed by a space (it can be at the end of
   --    the line).
   --
   --    A right paren must either be the first non-blank character on a line,
   --    or it must be preceded by a non-blank character.
   --
   --    A semicolon must not be preceded by a blank, and must not be followed
   --    by a non-blank character.
   --
   --    A unary plus or minus may not be followed by a space
   --
   --    There must be one blank (and no other white space) between NOT and IN
   --
   --    A vertical bar must be surrounded by spaces
   --
   --  Note that a requirement that a token be preceded by a space is met by
   --  placing the token at the start of the line, and similarly a requirement
   --  that a token be followed by a space is met by placing the token at
   --  the end of the line. Note that in the case where horizontal tabs are
   --  permitted, a horizontal tab is acceptable for meeting the requirement
   --  for a space.

   Style_Check_Xtra_Parens : Boolean := False;
   --  This can be set True by using the -gnatyx switch. If true, then it is
   --  not allowed to enclose entire expressions in tests in parentheses
   --  (C style), e.g. if (x = y) then ... is not allowed.

   Style_Max_Line_Length : Nat := 0;
   --  Value used to check maximum line length. Gets reset as a result of
   --  use of -gnatym or -gnatyMnnn switches. This value is only read if
   --  Style_Check_Max_Line_Length is True.

   Style_Max_Nesting_Level : Nat := 0;
   --  Value used to check maximum nesting level. Gets reset as a result
   --  of use of the -gnatyLnnn switch. This value is only read if
   --  Style_Check_Max_Nesting_Level is True.

   -----------------
   -- Subprograms --
   -----------------

   function RM_Column_Check return Boolean;
   --  Determines whether style checking is active and the RM column check
   --  mode is set requiring checking of RM format layout.

   procedure Set_Default_Style_Check_Options;
   --  This procedure is called to set the default style checking options in
   --  response to a -gnaty switch with no suboptions or from -gnatyy.

   procedure Set_GNAT_Style_Check_Options;
   --  This procedure is called to set the default style checking options for
   --  GNAT units (as set by -gnatg or -gnatyg).

   Style_Msg_Buf : String (1 .. 80);
   Style_Msg_Len : Natural;
   --  Used to return

   procedure Set_Style_Check_Options
     (Options  : String;
      OK       : out Boolean;
      Err_Col  : out Natural);
   --  This procedure is called to set the style check options that correspond
   --  to the characters in the given Options string. If all options are valid,
   --  they are set in an additive manner: any previous options are retained
   --  unless overridden, unless a minus is encountered, and then subsequent
   --  style switches are subtracted from the current set.
   --
   --  If all options given are valid, then OK is True, Err_Col is set to
   --  Options'Last + 1, and Style_Msg_Buf/Style_Msg_Len are unchanged.
   --
   --  If an invalid character is found, then OK is False on exit, and Err_Col
   --  is the index in options of the bad character. In this case Style_Msg_Len
   --  is set and Style_Msg_Buf (1 .. Style_Msg_Len) has a detailed message
   --  describing the error.

   procedure Set_Style_Check_Options (Options : String);
   --  Like the above procedure, but used when the Options string is known to
   --  be valid. This is for example appropriate for calls where the string was
   --  obtained by Save_Style_Check_Options.

   procedure Reset_Style_Check_Options;
   --  Sets all style check options to off

   subtype Style_Check_Options is String (1 .. 64);
   --  Long enough string to hold all options from Save call below

   procedure Save_Style_Check_Options (Options : out Style_Check_Options);
   --  Sets Options to represent current selection of options. This set can be
   --  restored by first calling Reset_Style_Check_Options, and then calling
   --  Set_Style_Check_Options with the Options string.

end Stylesw;

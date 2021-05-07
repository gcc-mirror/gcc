------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               E R R O U T                                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2021, Free Software Foundation, Inc.         --
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

--  This package contains the routines to output error messages. They are
--  basically system independent, however in some environments, e.g. when the
--  parser is embedded into an editor, it may be appropriate to replace the
--  implementation of this package.

with Err_Vars;
with Erroutc;
with Namet;    use Namet;
with Table;
with Types;    use Types;
with Uintp;    use Uintp;

with System;

package Errout is

   Current_Error_Source_File : Source_File_Index
     renames Err_Vars.Current_Error_Source_File;
   --  Id of current messages. Used to post file name when unit changes. This
   --  is initialized to Main_Source_File at the start of a compilation, which
   --  means that no file names will be output unless there are errors in
   --  units other than the main unit. However, if the main unit has a pragma
   --  Source_Reference line, then this is initialized to No_Source_File, to
   --  force an initial reference to the real source file name.

   Raise_Exception_On_Error : Nat renames Err_Vars.Raise_Exception_On_Error;
   --  If this value is non-zero, then any attempt to generate an error
   --  message raises the exception Error_Msg_Exception, and the error message
   --  is not output. This is used for defending against junk resulting from
   --  illegalities, and also for substitution of more appropriate error
   --  messages from higher semantic levels. It is a counter so that the
   --  increment/decrement protocol nests neatly.

   Error_Msg_Exception : exception renames Err_Vars.Error_Msg_Exception;
   --  Exception raised if Raise_Exception_On_Error is true

   Warning_Doc_Switch : Boolean renames Err_Vars.Warning_Doc_Switch;
   --  If this is set True, then the ??/?*?/?$?/?x?/?X? insertion sequences in
   --  error messages generate appropriate tags for the output error messages.
   --  If this switch is False, then these sequences are still recognized (for
   --  the purposes of implementing the pattern matching in pragmas Warnings
   --  (Off,..) and Warning_As_Pragma(...) but do not result in adding the
   --  error message tag. The -gnatw.d switch sets this flag True, -gnatw.D
   --  sets this flag False.

   Current_Node : Node_Id := Empty;
   --  Used by Error_Msg as a default Node_Id.
   --  Relevant only when Opt.Include_Subprogram_In_Messages is set.

   -----------------------------------
   -- Suppression of Error Messages --
   -----------------------------------

   --  In an effort to reduce the impact of redundant error messages, the
   --  error output routines in this package normally suppress certain
   --  classes of messages as follows:

   --    1.  Identical messages placed at the same point in the text. Such
   --        duplicate error message result for example from rescanning
   --        sections of the text that contain lexical errors. Only one of
   --        such a set of duplicate messages is output, and the rest are
   --        suppressed.

   --    2.  If more than one parser message is generated for a single source
   --        line, then only the first message is output, the remaining
   --        messages on the same line are suppressed.

   --    3.  If a message is posted on a node for which a message has been
   --        previously posted, then only the first message is retained. The
   --        Error_Posted flag is used to detect such multiple postings. Note
   --        that this only applies to semantic messages, since otherwise
   --        for parser messages, this would be a special case of case 2.

   --    4.  If a message is posted on a node whose Etype or Entity
   --        fields reference entities on which an error message has
   --        already been placed, as indicated by the Error_Posted flag
   --        being set on these entities, then the message is suppressed.

   --    5.  If a message attempts to insert an Error node, or a direct
   --        reference to the Any_Type node, then the message is suppressed.

   --    6.  Note that cases 2-5 only apply to error messages, not warning
   --        messages. Warning messages are only suppressed for case 1, and
   --        when they come from other than the main extended unit.

   --    7.  If an error or warning references an internal name, and we have
   --        already placed an error (not warning) message at that location,
   --        then we assume this is cascaded junk and delete the message.

   --  This normal suppression action may be overridden in cases 2-5 (but
   --  not in case 1 or 7) by setting All_Errors mode, or by setting the
   --  unconditional message insertion character (!) as described below.

   ---------------------------------------------------------
   -- Error Message Text and Message Insertion Characters --
   ---------------------------------------------------------

   --  Error message text strings are composed of lower case letters, digits
   --  and the special characters space, comma, period, colon and semicolon,
   --  apostrophe and parentheses. Special insertion characters can also
   --  appear which cause the error message circuit to modify the given
   --  string as follows:

   --    Insertion character % (Percent: insert name from Names table)
   --      The character % is replaced by the text for the name specified by
   --      the Name_Id value stored in Error_Msg_Name_1. A blank precedes the
   --      name if it is preceded by a non-blank character other than left
   --      parenthesis. The name is enclosed in quotes unless manual quotation
   --      mode is set. If the Name_Id is set to No_Name, then no insertion
   --      occurs; if the Name_Id is set to Error_Name, then the string
   --      <error> is inserted. A second and third % may appear in a single
   --      message, similarly replaced by the names which are specified by the
   --      Name_Id values stored in Error_Msg_Name_2 and Error_Msg_Name_3. The
   --      names are decoded and cased according to the current identifier
   --      casing mode. Note: if a unit name ending with %b or %s is passed
   --      for this kind of insertion, this suffix is simply stripped. Use a
   --      unit name insertion ($) to process the suffix.
   --
   --      Note: the special names _xxx (xxx = Pre/Post/Invariant) are changed
   --      to insert the string xxx'Class into the message.

   --    Insertion character %% (Double percent: insert literal name)
   --      The character sequence %% acts as described above for %, except
   --      that the name is simply obtained with Get_Name_String and is not
   --      decoded or cased, it is inserted literally from the names table.
   --      A trailing %b or %s is not treated specially.
   --
   --      Note: the special names _xxx (xxx = Pre/Post/Invariant) are changed
   --      to insert the string xxx'Class into the message.

   --    Insertion character $ (Dollar: insert unit name from Names table)
   --      The character $ is treated similarly to %, except that the name is
   --      obtained from the Unit_Name_Type value in Error_Msg_Unit_1 and
   --      Error_Msg_Unit_2, as provided by Get_Unit_Name_String in package
   --      Uname. Note that this name includes the postfix (spec) or (body)
   --      strings. If this postfix is not required, use the normal % insertion
   --      for the unit name.

   --    Insertion character { (Left brace: insert file name from names table)
   --      The character { is treated similarly to %, except that the input
   --      value is a File_Name_Type value stored in Error_Msg_File_1 or
   --      Error_Msg_File_2 or Error_Msg_File_3. The value is output literally,
   --      enclosed in quotes as for %, but the case is not modified, the
   --      insertion is the exact string stored in the names table without
   --      adjusting the casing.

   --    Insertion character * (Asterisk: insert reserved word name)
   --      The insertion character * is treated exactly like % except that the
   --      resulting name is cased according to the default conventions for
   --      reserved words (see package Scans).

   --    Insertion character & (Ampersand: insert name from node)
   --      The insertion character & is treated similarly to %, except that
   --      the name is taken from the Chars field of the given node, and may
   --      refer to a child unit name, or a selected component. The casing is,
   --      if possible, taken from the original source reference, which is
   --      obtained from the Sloc field of the given node or nodes. If no Sloc
   --      is available (happens e.g. for nodes in package Standard), then the
   --      default case (see Scans spec) is used. The nodes to be used are
   --      stored in Error_Msg_Node_1, Error_Msg_Node_2. No insertion occurs
   --      for the Empty node, and the Error node results in the insertion of
   --      the characters <error>. In addition, if the special global variable
   --      Error_Msg_Qual_Level is non-zero, then the reference will include
   --      up to the given number of levels of qualification, using the scope
   --      chain.
   --
   --      Note: the special names _xxx (xxx = Pre/Post/Invariant) are changed
   --      to insert the string xxx'Class into the message.

   --    Insertion character # (Pound: insert line number reference)
   --      The character # is replaced by the string indicating the source
   --      position stored in Error_Msg_Sloc. There are three cases:
   --
   --        for package Standard:           in package Standard
   --        for locations in current file:  at line nnn:ccc
   --        for locations in other files:   at filename:nnn:ccc
   --
   --      By convention, the # insertion character is only used at the end of
   --      an error message, so the above strings only appear as the last
   --      characters of an error message. The only exceptions to this rule
   --      are that an RM reference may follow in the form (RM .....) and a
   --      right parenthesis may immediately follow the #. In the case of
   --      continued messages, # can only appear at the end of a group of
   --      continuation messages, except that \\ messages which always start
   --      a new line end the sequence from the point of view of this rule.
   --      The idea is that for any use of -gnatj, it will still be the case
   --      that a location reference appears only at the end of a line.

   --      Note: the output of the string "at " is suppressed if the string
   --      " from" or " from " immediately precedes the insertion character #.
   --      Certain messages read better with from than at.

   --    Insertion character } (Right brace: insert type reference)
   --      The character } is replaced by a string describing the type
   --      referenced by the entity whose Id is stored in Error_Msg_Node_1.
   --      The string gives the name or description of the type, and also
   --      where appropriate the location of its declaration. Special cases
   --      like "some integer type" are handled appropriately. Only one } is
   --      allowed in a message, since there is not enough room for two (the
   --      insertion can be quite long, including a file name). In addition, if
   --      the special global variable Error_Msg_Qual_Level is non-zero, then
   --      the reference will include up to the given number of levels of
   --      qualification, using the scope chain.

   --    Insertion character @ (At: insert column number reference)
   --      The character @ is replaced by null if the RM_Column_Check mode is
   --      off (False). If the switch is on (True), then @ is replaced by the
   --      text string " in column nnn" where nnn is the decimal
   --      representation of the column number stored in Error_Msg_Col plus
   --      one (the plus one is because the number is stored 0-origin and
   --      displayed 1-origin).

   --    Insertion character ^ (Caret: insert integer value)
   --      The character ^ is replaced by the decimal conversion of the Uint
   --      value stored in Error_Msg_Uint_1, with a possible leading minus.
   --      A second ^ may occur in the message, in which case it is replaced
   --      by the decimal conversion of the Uint value in Error_Msg_Uint_2.

   --    Insertion character > (Greater Than: run time name)
   --      The character > is replaced by a string of the form (name) if
   --      Targparm scanned out a Run_Time_Name (see package Targparm for
   --      details). The name is enclosed in parentheses and output in mixed
   --      case mode (upper case after any space in the name). If no run time
   --      name is defined, this insertion character has no effect.

   --    Insertion character ! (Exclamation: unconditional message)
   --      The character ! appearing anywhere in the text of a message makes
   --      the message unconditional which means that it is output even if it
   --      would normally be suppressed. See section above for a description
   --      of the cases in which messages are normally suppressed. Note that
   --      in the case of warnings, the meaning is that the warning should not
   --      be removed in dead code (that's the only time that the use of !
   --      has any effect for a warning).
   --
   --      Note: the presence of ! is ignored in continuation messages (i.e.
   --      messages starting with the \ insertion character). The effect of the
   --      use of ! in a parent message automatically applies to all of its
   --      continuation messages (since we clearly don't want any case in which
   --      continuations are separated from the main message). It is allowable
   --      to put ! in continuation messages, and the usual style is to include
   --      it, since it makes it clear that the continuation is part of an
   --      unconditional message.

   --    Insertion character !! (Double exclamation: unconditional warning)
   --      Normally warning messages issued in other than the main unit are
   --      suppressed. If the message contains !! then this suppression is
   --      avoided. This is currently used by the Compile_Time_Warning pragma
   --      to ensure the message for a with'ed unit is output, and for warnings
   --      on ineffective back-end inlining, which is detected in units that
   --      contain subprograms to be inlined in the main program. It is also
   --      used by the Compiler_Unit_Warning pragma for similar reasons.

   --    Insertion character ? (Question: warning message)
   --      The character ? appearing anywhere in a message makes the message
   --      warning instead of a normal error message, and the text of the
   --      message will be preceded by "warning:" in the normal case. The
   --      handling of warnings if further controlled by the Warning_Mode
   --      option (-w switch), see package Opt for further details, and also by
   --      the current setting from pragma Warnings. This pragma applies only
   --      to warnings issued from the semantic phase (not the parser), but
   --      currently all relevant warnings are posted by the semantic phase
   --      anyway. Messages starting with (style) are also treated as warning
   --      messages.
   --
   --      Note: when a warning message is output, the text of the message is
   --      preceded by "warning: " in the normal case. An exception to this
   --      rule occurs when the text of the message starts with "info: " in
   --      which case this string is not prepended. This allows callers to
   --      label certain warnings as informational messages, rather than as
   --      warning messages requiring some action.
   --
   --      Note: the presence of ? is ignored in continuation messages (i.e.
   --      messages starting with the \ insertion character). The warning
   --      status of continuations is determined only by the parent message
   --      which is being continued. It is allowable to put ? in continuation
   --      messages, and the usual style is to include it, since it makes it
   --      clear that the continuation is part of a warning message, but it is
   --      not necessary to go through any computational effort to include it.
   --
   --      Note: this usage is obsolete, use ?? ?*? ?$? ?x? ?X? to specify
   --      the string to be added when Warn_Doc_Switch is set to True. If this
   --      switch is True, then for simple ? messages it has no effect. This
   --      simple form is to ease transition and may be removed later except
   --      for GNATprove-specific messages (info and warnings) which are not
   --      subject to the same GNAT warning switches.

   --    Insertion character ?? (Two question marks: default warning)
   --      Like ?, but if the flag Warn_Doc_Switch is True, adds the string
   --      "[enabled by default]" at the end of the warning message. For
   --      continuations, use this in each continuation message.

   --    Insertion character ?x? (warning with switch)
   --      Like ?, but if the flag Warn_Doc_Switch is True, adds the string
   --      "[-gnatwx]" at the end of the warning message. x is a lower case
   --      letter. For continuations, use this on each continuation message.

   --    Insertion character ?X? (warning with dot switch)
   --      Like ?, but if the flag Warn_Doc_Switch is True, adds the string
   --      "[-gnatw.x]" at the end of the warning message. X is an upper case
   --      letter corresponding to the lower case letter x in the message.
   --      For continuations, use this on each continuation message.

   --    Insertion character ?*? (restriction warning)
   --      Like ?, but if the flag Warn_Doc_Switch is True, adds the string
   --      "[restriction warning]" at the end of the warning message. For
   --      continuations, use this on each continuation message.

   --    Insertion character ?$? (elaboration informational messages)
   --      Like ?, but if the flag Warn_Doc_Switch is True, adds the string
   --      "[-gnatel]" at the end of the info message. This is used for the
   --      messages generated by the switch -gnatel. For continuations, use
   --      this on each continuation message.

   --    Insertion character < (Less Than: conditional warning message)
   --      The character < appearing anywhere in a message is used for a
   --      conditional error message. If Error_Msg_Warn is True, then the
   --      effect is the same as ? described above, and in particular << <X<
   --      <x< <$< <*< have the effect of ?? ?X? ?x? ?$? ?*? respectively. If
   --      Error_Msg_Warn is False, then the < << or <X< sequence is ignored
   --      and the message is treated as a error rather than a warning.

   --    Insertion character A-Z (Upper case letter: Ada reserved word)
   --      If two or more upper case letters appear in the message, they are
   --      taken as an Ada reserved word, and are converted to the default
   --      case for reserved words (see Scans package spec). Surrounding
   --      quotes are added unless manual quotation mode is currently set.
   --      RM and SPARK are special exceptions, they are never treated as
   --      keywords, and just appear verbatim, with no surrounding quotes.
   --      As a special case, 'R'M is used instead of RM (which is not treated
   --      as a keyword) to indicate when the reference to the RM is possibly
   --      not useful anymore, and could possibly be replaced by a comment
   --      in the source.

   --    Insertion character ` (Backquote: set manual quotation mode)
   --      The backquote character always appears in pairs. Each backquote of
   --      the pair is replaced by a double quote character. In addition, any
   --      reserved keywords, or name insertions between these backquotes are
   --      not surrounded by the usual automatic double quotes. See the
   --      section below on manual quotation mode for further details.

   --    Insertion character ' (Quote: literal character)
   --      Precedes a character which is placed literally into the message.
   --      Used to insert characters into messages that are one of the
   --      insertion characters defined here. Also used for insertion of
   --      upper case letter sequences not to be treated as keywords.

   --    Insertion character \ (Backslash: continuation message)
   --      Indicates that the message is a continuation of a message
   --      previously posted. This is used to ensure that such groups of
   --      messages are treated as a unit. The \ character must be the first
   --      character of the message text.

   --    Insertion character \\ (Two backslashes: continuation with new line)
   --      This differs from \ only in -gnatjnn mode (Error_Message_Line_Length
   --      set non-zero). This sequence forces a new line to start even when
   --      continuations are being gathered into a single message.

   --    Insertion character | (Vertical bar: non-serious error)
   --      By default, error messages (but not warning messages) are considered
   --      to be fatal error messages, which prevent expansion and generation
   --      of code. If the insertion character | appears, the message is
   --      considered to be nonserious, and Serious_Errors_Detected is not
   --      incremented, so expansion is not prevented by such a msg. This
   --      insertion character is ignored in continuation messages.

   --    Insertion character ~ (Tilde: insert string)
   --      Indicates that Error_Msg_String (1 .. Error_Msg_Strlen) is to be
   --      inserted to replace the ~ character. The string is inserted in the
   --      literal form it appears, without any action on special characters.

   --    Insertion character [ (Left bracket: will/would be raised at run time)
   --      This is used in messages about exceptions being raised at run-time.
   --      If the current message is a warning message, then if the code is
   --      executed, the exception will be raised, and [ inserts:
   --
   --        will be raised at run time
   --
   --      If the current message is an error message, then it is an error
   --      because the exception would have been raised and [ inserts:
   --
   --        would have been raised at run time
   --
   --      Typically the message contains a < insertion which means that the
   --      message is a warning or error depending on Error_Msg_Warn. This is
   --      most typically used in the context of messages which are normally
   --      warnings, but are errors in GNATprove mode, corresponding to the
   --      permission in the definition of SPARK that allows an implementation
   --      to reject a program as illegal if a situation arises in which the
   --      compiler can determine that it is certain that a run-time check
   --      would have fail if the statement was executed.

   --    Insertion character ] (Right bracket: may/might be raised at run time)
   --      This is like [ except that the insertion messages say may/might,
   --      instead of will/would.

   --    Insertion sequence "(style)" (style message)
   --      This appears only at the start of the message (and not any of its
   --      continuations, if any), and indicates that the message is a style
   --      message. Style messages are also considered to be warnings, but
   --      they do not get a tag.

   --    Insertion sequence "info: " (informational message)
   --      This appears only at the start of the message (and not any of its
   --      continuations, if any), and indicates that the message is an info
   --      message. The message will be output with this prefix, and if there
   --      are continuations that are not printed using the -gnatj switch they
   --      will also have this prefix. Informational messages are usually also
   --      warnings, but they don't have to be.

   --    Insertion sequence "low: " or "medium: " or "high: " (check message)
   --      This appears only at the start of the message (and not any of its
   --      continuations, if any), and indicates that the message is a check
   --      message. The message will be output with this prefix. Check
   --      messages are not fatal (so are like info messages in that respect)
   --      and are not controlled by pragma Warnings.

   -----------------------------------------------------
   -- Global Values Used for Error Message Insertions --
   -----------------------------------------------------

   --  The following global variables are essentially additional parameters
   --  passed to the error message routine for insertion sequences described
   --  above. The reason these are passed globally is that the insertion
   --  mechanism is essentially an untyped one in which the appropriate
   --  variables are set depending on the specific insertion characters used.

   --  Note that is mandatory that the caller ensure that global variables
   --  are set before the Error_Msg call, otherwise the result is undefined.

   --  Also note that calls to Error_Msg and its variants destroy the value of
   --  these global variables, as a way to support the inclusion of multiple
   --  insertion characters of the same type. For example, support for
   --  multiple characters % for a name in the message (up to 3) is
   --  implemented by unconditionally shifting the value for Error_Msg_Nam_2
   --  to Error_Msg_Nam_1 and from Error_Msg_Nam_3 to Error_Msg_Nam_2 after
   --  dealing with insertion character %. The caller should ensure that all
   --  global variables are restored if needed prior to calling Error_Msg.

   Error_Msg_Col : Column_Number renames Err_Vars.Error_Msg_Col;
   --  Column for @ insertion character in message

   Error_Msg_Uint_1 : Uint renames Err_Vars.Error_Msg_Uint_1;
   Error_Msg_Uint_2 : Uint renames Err_Vars.Error_Msg_Uint_2;
   --  Uint values for ^ insertion characters in message

   Error_Msg_Sloc : Source_Ptr renames Err_Vars.Error_Msg_Sloc;
   --  Source location for # insertion character in message

   Error_Msg_Name_1 : Name_Id renames Err_Vars.Error_Msg_Name_1;
   Error_Msg_Name_2 : Name_Id renames Err_Vars.Error_Msg_Name_2;
   Error_Msg_Name_3 : Name_Id renames Err_Vars.Error_Msg_Name_3;
   --  Name_Id values for % insertion characters in message

   Error_Msg_File_1 : File_Name_Type renames Err_Vars.Error_Msg_File_1;
   Error_Msg_File_2 : File_Name_Type renames Err_Vars.Error_Msg_File_2;
   Error_Msg_File_3 : File_Name_Type renames Err_Vars.Error_Msg_File_3;
   --  File_Name_Type values for { insertion characters in message

   Error_Msg_Unit_1 : Unit_Name_Type renames Err_Vars.Error_Msg_Unit_1;
   Error_Msg_Unit_2 : Unit_Name_Type renames Err_Vars.Error_Msg_Unit_2;
   --  Unit_Name_Type values for $ insertion characters in message

   Error_Msg_Node_1 : Node_Id renames Err_Vars.Error_Msg_Node_1;
   Error_Msg_Node_2 : Node_Id renames Err_Vars.Error_Msg_Node_2;
   --  Node_Id values for & insertion characters in message

   Error_Msg_Qual_Level : Nat renames Err_Vars.Error_Msg_Qual_Level;
   --  Number of levels of qualification required for type name (see the
   --  description of the } insertion character). Note that this value does
   --  not get reset by any Error_Msg call, so the caller is responsible
   --  for resetting it.

   Error_Msg_Warn : Boolean renames Err_Vars.Error_Msg_Warn;
   --  Used if current message contains a < insertion character to indicate
   --  if the current message is a warning message. Must be set appropriately
   --  before any call to Error_Msg_xxx with a < insertion character present.
   --  Setting is irrelevant if no < insertion character is present.

   Error_Msg_String : String  renames Err_Vars.Error_Msg_String;
   Error_Msg_Strlen : Natural renames Err_Vars.Error_Msg_Strlen;
   --  Used if current message contains a ~ insertion character to indicate
   --  insertion of the string Error_Msg_String (1 .. Error_Msg_Strlen).

   -----------------------------------------------------
   -- Format of Messages and Manual Quotation Control --
   -----------------------------------------------------

   --  Messages are generally all in lower case, except for inserted names
   --  and appear in one of the following three forms:

   --    error: text
   --    warning: text

   --  The prefixes error and warning are supplied automatically (depending
   --  on the use of the ? insertion character), and the call to the error
   --  message routine supplies the text. The "error: " prefix is omitted
   --  in brief error message formats.

   --  Reserved Ada keywords in the message are in the default keyword case
   --  (determined from the given source program), surrounded by quotation
   --  marks. This is achieved by spelling the reserved word in upper case
   --  letters, which is recognized as a request for insertion of quotation
   --  marks by the error text processor. Thus for example:

   --    Error_Msg_AP ("IS expected");

   --  would result in the output of one of the following:

   --    error: "is" expected
   --    error: "IS" expected
   --    error: "Is" expected

   --  the choice between these being made by looking at the casing convention
   --  used for keywords (actually the first compilation unit keyword) in the
   --  source file.

   --  Note: a special exception is that RM is never treated as a keyword
   --  but instead is copied literally into the message, this avoids the
   --  need for writing 'R'M for all reference manual quotes. A similar
   --  exception is applied to the occurrence of the string SPARK used in
   --  error messages about the SPARK subset of Ada.

   --  In the case of names, the default mode for the error text processor
   --  is to surround the name by quotation marks automatically. The case
   --  used for the identifier names is taken from the source program where
   --  possible, and otherwise is the default casing convention taken from
   --  the source file usage.

   --  In some cases, better control over the placement of quote marks is
   --  required. This is achieved using manual quotation mode. In this mode,
   --  one or more insertion sequences is surrounded by backquote characters.
   --  The backquote characters are output as double quote marks, and normal
   --  automatic insertion of quotes is suppressed between the double quotes.
   --  For example:

   --    Error_Msg_AP ("`END &;` expected");

   --  generates a message like

   --    error: "end Open_Scope;" expected

   --  where the node specifying the name Open_Scope has been stored in
   --  Error_Msg_Node_1 prior to the call. The great majority of error
   --  messages operates in normal quotation mode.

   --  Note: the normal automatic insertion of spaces before insertion
   --  sequences (such as those that come from & and %) is suppressed in
   --  manual quotation mode, so blanks, if needed as in the above example,
   --  must be explicitly present.

   ----------------------------
   -- Message ID Definitions --
   ----------------------------

   subtype Error_Msg_Id is Erroutc.Error_Msg_Id;
   function "=" (Left, Right : Error_Msg_Id) return Boolean
     renames Erroutc."=";
   --  A type used to represent specific error messages. Used by the clients
   --  of this package only in the context of the Get_Error_Id and
   --  Change_Error_Text subprograms.

   No_Error_Msg : constant Error_Msg_Id := Erroutc.No_Error_Msg;
   --  A constant which is different from any value returned by Get_Error_Id.
   --  Typically used by a client to indicate absense of a saved Id value.

   Warning_Msg : Error_Msg_Id := No_Error_Msg;
   --  This is set if a warning message is generated to the ID of the resulting
   --  message. Continuation messages have no effect. It is legitimate for the
   --  client to set this to No_Error_Msg and then test it to see if a warning
   --  message has been issued.

   procedure Delete_Warning_And_Continuations (Msg : Error_Msg_Id);
   --  Deletes the given warning message and all its continuations. This is
   --  typically used in conjunction with reading the value of Warning_Msg.

   function Get_Msg_Id return Error_Msg_Id renames Erroutc.Get_Msg_Id;
   --  Returns the Id of the message most recently posted using one of the
   --  Error_Msg routines.

   function Get_Location (E : Error_Msg_Id) return Source_Ptr
     renames Erroutc.Get_Location;
   --  Returns the flag location of the error message with the given id E

   ------------------------
   -- List Pragmas Table --
   ------------------------

   --  When a pragma Page or pragma List is encountered by the parser, an
   --  entry is made in the following table. This table is then used to
   --  control the full listing if one is being generated. Note that the
   --  reason we do the processing in the parser is so that we get proper
   --  listing control even in syntax check only mode.

   type List_Pragma_Type is (List_On, List_Off, Page);

   type List_Pragma_Record is record
      Ptyp : List_Pragma_Type;
      Ploc : Source_Ptr;
   end record;

   --  Note: Ploc points to the terminating semicolon in the List_Off and Page
   --  cases, and to the pragma keyword for List_On. In the case of a pragma
   --  List_Off, a List_On entry is also made in the table, pointing to the
   --  pragma keyword. This ensures that, as required, a List (Off) pragma is
   --  listed even in list off mode.

   package List_Pragmas is new Table.Table (
     Table_Component_Type => List_Pragma_Record,
     Table_Index_Type     => Int,
     Table_Low_Bound      => 1,
     Table_Initial        => 50,
     Table_Increment      => 200,
     Table_Name           => "List_Pragmas");

   ---------------------------
   -- Ignore_Errors Feature --
   ---------------------------

   --  In certain cases, notably for optional subunits, the compiler operates
   --  in a mode where errors are to be ignored, and the whole unit is to be
   --  considered as not present. To implement this we provide the following
   --  flag to enable special handling, where error messages are suppressed,
   --  but the Fatal_Error flag will still be set in the normal manner.

   Ignore_Errors_Enable : Nat := 0;
   --  Triggering switch. If non-zero, then ignore errors mode is activated.
   --  This is a counter to allow convenient nesting of enable/disable.

   -----------------------
   --  CODEFIX Facility --
   -----------------------

   --  The GNAT Studio and GNATBench IDE's have a codefix facility that allows
   --  for automatic correction of a subset of the errors and warnings issued
   --  by the compiler. This is done by recognizing the text of specific
   --  messages using appropriate matching patterns.

   --  The text of such messages should not be altered without coordinating
   --  with the codefix code. All such messages are marked by a specific
   --  style of comments, as shown by the following example:

   --     Error_Msg_N -- CODEFIX
   --       (parameters ....)

   --  Any message marked with this -- CODEFIX comment should not be modified
   --  without appropriate coordination.

   ------------------------------
   -- Error Output Subprograms --
   ------------------------------

   procedure Initialize;
   --  Initializes for output of error messages. Must be called for each
   --  source file before using any of the other routines in the package.

   procedure Finalize (Last_Call : Boolean);
   --  Finalize processing of error message list. Includes processing for
   --  duplicated error messages, and other similar final adjustment of the
   --  list of error messages. Note that this procedure must be called before
   --  calling Compilation_Errors to determine if there were any errors. It
   --  is perfectly fine to call Finalize more than once, providing that the
   --  parameter Last_Call is set False for every call except the last call.

   --  This multiple call capability is used to do some processing that may
   --  generate messages. Call Finalize to eliminate duplicates and remove
   --  deleted warnings. Test for compilation errors using Compilation_Errors,
   --  then generate some more errors/warnings, call Finalize again to make
   --  sure that all duplicates in these new messages are dealt with, then
   --  finally call Output_Messages to output the final list of messages. The
   --  argument Last_Call must be set False on all calls except the last call,
   --  and must be set True on the last call (a value of True activates some
   --  processing that must only be done after all messages are posted).

   procedure Output_Messages;
   --  Output list of messages, including messages giving number of detected
   --  errors and warnings.

   procedure Error_Msg
     (Msg : String; Flag_Location : Source_Ptr);
   procedure Error_Msg
     (Msg : String; Flag_Span : Source_Span);
   procedure Error_Msg
     (Msg : String; Flag_Location : Source_Ptr; N : Node_Id);
   procedure Error_Msg
     (Msg : String; Flag_Span : Source_Span; N : Node_Id);
   --  Output a message at specified location. Can be called from the parser
   --  or the semantic analyzer. If N is set, points to the relevant node for
   --  this message. The version with a span is preferred whenever possible,
   --  in other cases the version with a location can still be used.

   procedure Error_Msg
     (Msg                    : String;
      Flag_Location          : Source_Ptr;
      Is_Compile_Time_Pragma : Boolean);
   --  Same as Error_Msg (String, Source_Ptr) except Is_Compile_Time_Pragma
   --  lets the caller specify whether this is a Compile_Time_Warning or
   --  Compile_Time_Error pragma.

   procedure Error_Msg_S (Msg : String);
   --  Output a message at current scan pointer location. This routine can be
   --  called only from the parser, since it references Scan_Ptr.

   procedure Error_Msg_AP (Msg : String);
   --  Output a message just after the previous token. This routine can be
   --  called only from the parser, since it references Prev_Token_Ptr.

   procedure Error_Msg_BC (Msg : String);
   --  Output a message just before the current token. Note that the important
   --  difference between this and the previous routine is that the BC case
   --  posts a flag on the current line, whereas AP can post a flag at the
   --  end of the preceding line. This routine can be called only from the
   --  parser, since it references Token_Ptr.

   procedure Error_Msg_SC (Msg : String);
   --  Output a message at the start of the current token, unless we are at
   --  the end of file, in which case we always output the message after the
   --  last real token in the file. This routine can be called only from the
   --  parser, since it references Token_Ptr.

   procedure Error_Msg_SP (Msg : String);
   --  Output a message at the start of the previous token. This routine can
   --  be called only from the parser, since it references Prev_Token_Ptr.

   procedure Error_Msg_N (Msg : String; N : Node_Or_Entity_Id);
   --  Output a message at the Sloc of the given node. This routine can be
   --  called from the parser or the semantic analyzer, although the call from
   --  the latter is much more common (and is the most usual way of generating
   --  error messages from the analyzer). The message text may contain a
   --  single & insertion, which will reference the given node. The message is
   --  suppressed if the node N already has a message posted, or if it is a
   --  warning and N is an entity node for which warnings are suppressed.

   --  WARNING: There is a matching C declaration of this subprogram in fe.h

   procedure Error_Msg_F (Msg : String; N : Node_Id);
   --  Similar to Error_Msg_N except that the message is placed on the first
   --  node of the construct N (First_Node (N)). Note that this procedure uses
   --  Original_Node to look at the original source tree, since that's what we
   --  want for placing an error message flag in the right place.

   procedure Error_Msg_NE
     (Msg : String;
      N   : Node_Or_Entity_Id;
      E   : Node_Or_Entity_Id);
   --  Output a message at the Sloc of the given node N, with an insertion of
   --  the name from the given entity node E. This is used by the semantic
   --  routines, where this is a common error message situation. The Msg text
   --  will contain a & or } as usual to mark the insertion point. This
   --  routine can be called from the parser or the analyzer.

   --  WARNING: There is a matching C declaration of this subprogram in fe.h

   procedure Error_Msg_FE
     (Msg : String;
      N   : Node_Id;
      E   : Node_Or_Entity_Id);
   --  Same as Error_Msg_NE, except that the message is placed on the first
   --  node of the construct N (First_Node (N)).

   procedure Error_Msg_NEL
     (Msg           : String;
      N             : Node_Or_Entity_Id;
      E             : Node_Or_Entity_Id;
      Flag_Location : Source_Ptr);
   procedure Error_Msg_NEL
     (Msg       : String;
      N         : Node_Or_Entity_Id;
      E         : Node_Or_Entity_Id;
      Flag_Span : Source_Span);
   --  Exactly the same as Error_Msg_NE, except that the flag is placed at
   --  the specified Flag_Location/Flag_Span instead of at Sloc (N).

   procedure Error_Msg_NW
     (Eflag : Boolean;
      Msg   : String;
      N     : Node_Or_Entity_Id);
   --  This routine is used for posting a message conditionally. The message
   --  is posted (with the same effect as Error_Msg_N (Msg, N) if and only
   --  if Eflag is True and if the node N is within the main extended source
   --  unit and comes from source. Typically this is a warning mode flag.
   --  This routine can only be called during semantic analysis. It may not
   --  be called during parsing.

   procedure Change_Error_Text (Error_Id : Error_Msg_Id; New_Msg : String);
   --  The error message text of the message identified by Id is replaced by
   --  the given text. This text may contain insertion characters in the
   --  usual manner, and need not be the same length as the original text.

   procedure First_And_Last_Nodes
     (C                     : Node_Id;
      First_Node, Last_Node : out Node_Id);
   --  Given a construct C, finds the first and last node in the construct,
   --  i.e. the ones with the lowest and highest Sloc value. This is useful in
   --  placing error msgs. Note that this procedure uses Original_Node to look
   --  at the original source tree, since that's what we want for placing an
   --  error message flag in the right place.

   function First_Node (C : Node_Id) return Node_Id;
   --  Return the first output of First_And_Last_Nodes

   function First_Sloc (N : Node_Id) return Source_Ptr;
   --  Given the node for an expression, return a source pointer value that
   --  points to the start of the first token in the expression. In the case
   --  where the expression is parenthesized, an attempt is made to include
   --  the parentheses (i.e. to return the location of the initial paren).

   function Get_Ignore_Errors return Boolean;
   --  Return True if all error calls are ignored.

   function Last_Node (C : Node_Id) return Node_Id;
   --  Return the last output of First_And_Last_Nodes

   function Last_Sloc (N : Node_Id) return Source_Ptr;
   --  Given the node for an expression, return a source pointer value that
   --  points to the end of the last token in the expression. In the case
   --  where the expression is parenthesized, an attempt is made to include
   --  the parentheses (i.e. to return the location of the final paren).

   procedure Purge_Messages (From : Source_Ptr; To : Source_Ptr)
     renames Erroutc.Purge_Messages;
   --  All error messages whose location is in the range From .. To (not
   --  including the end points) will be deleted from the error listing.

   procedure Remove_Warning_Messages (N : Node_Id);
   --  Remove any warning messages corresponding to the Sloc of N or any
   --  of its descendant nodes. No effect if no such warnings. Note that
   --  style messages (identified by the fact that they start with "(style)")
   --  are not removed by this call. Basically the idea behind this procedure
   --  is to remove warnings about execution conditions from known dead code.

   procedure Remove_Warning_Messages (L : List_Id);
   --  Remove warnings on all elements of a list (Calls Remove_Warning_Messages
   --  on each element of the list, see above).

   procedure Reset_Warnings;
   --  Reset the counts related to warnings. This is used both to initialize
   --  these counts and to reset them after each phase of analysis for a given
   --  value of Opt.Warning_Mode in gnat2why.

   procedure Set_Ignore_Errors (To : Boolean);
   --  Following a call to this procedure with To=True, all error calls are
   --  ignored. A call with To=False restores the default treatment in which
   --  error calls are treated as usual (and as described in this spec).

   procedure Set_Warnings_Mode_Off (Loc : Source_Ptr; Reason : String_Id)
     renames Erroutc.Set_Warnings_Mode_Off;
   --  Called in response to a pragma Warnings (Off) to record the source
   --  location from which warnings are to be turned off. Reason is the
   --  Reason from the pragma, or the null string if none is given.

   procedure Set_Warnings_Mode_On (Loc : Source_Ptr)
     renames Erroutc.Set_Warnings_Mode_On;
   --  Called in response to a pragma Warnings (On) to record the source
   --  location from which warnings are to be turned back on.

   procedure Set_Specific_Warning_Off
     (Loc    : Source_Ptr;
      Msg    : String;
      Reason : String_Id;
      Config : Boolean;
      Used   : Boolean := False)
     renames Erroutc.Set_Specific_Warning_Off;
   --  This is called in response to the two argument form of pragma Warnings
   --  where the first argument is OFF, and the second argument is a string
   --  which identifies a specific warning to be suppressed. The first argument
   --  is the start of the suppression range, and the second argument is the
   --  string from the pragma. Loc is the location of the pragma (which is the
   --  start of the range to suppress). Reason is the reason string from the
   --  pragma, or the null string if no reason is given. Config is True for the
   --  configuration pragma case (where there is no requirement for a matching
   --  OFF pragma). Used is set True to disable the check that the warning
   --  actually has the effect of suppressing a warning.

   procedure Set_Specific_Warning_On
     (Loc : Source_Ptr;
      Msg : String;
      Err : out Boolean)
     renames Erroutc.Set_Specific_Warning_On;
   --  This is called in response to the two argument form of pragma Warnings
   --  where the first argument is ON, and the second argument is the prefix
   --  of a specific warning to be suppressed. The first argument is the end
   --  of the suppression range, and the second argument is the string from
   --  the pragma. Err is set to True on return to report the error of no
   --  matching Warnings Off pragma preceding this one.

   function Compilation_Errors return Boolean;
   --  Returns True if errors have been detected, or warnings in -gnatwe (treat
   --  warnings as errors) mode. Note that it is mandatory to call Finalize
   --  before calling this routine. To account for changes to Warning_Mode in
   --  gnat2why between phases, the past or current presence of an error is
   --  recorded in a global variable at each call.

   procedure Error_Msg_CRT (Feature : String; N : Node_Id);
   --  Posts a non-fatal message on node N saying that the feature identified
   --  by the Feature argument is not supported in either configurable
   --  run-time mode or no run-time mode (as appropriate). In the former case,
   --  the name of the library is output if available.

   procedure Error_Msg_PT (E : Entity_Id; Iface_Prim : Entity_Id);
   --  Posts an error on protected type entry or subprogram E (referencing its
   --  overridden interface primitive Iface_Prim) indicating wrong mode of the
   --  first formal (RM 9.4(11.9/3)).

   procedure Error_Msg_Ada_2005_Extension (Extension : String);
   --  Analogous to Error_Msg_Ada_2012_Feature, but phrase the message using
   --  "extension" and not "feature". This routine is only used in the parser,
   --  so the error is always placed at the Token_Ptr.

   procedure Error_Msg_Ada_2012_Feature (Feature : String; Loc : Source_Ptr);
   --  If not operating in Ada 2012 mode or higher, posts errors complaining
   --  that Feature is only supported in Ada 2012, with appropriate suggestions
   --  to fix this. Loc is the location at which the flag is to be posted.
   --  Feature, which appears at the start of the first generated message, may
   --  contain error message insertion characters in the normal manner, and in
   --  particular may start with | to flag a non-serious error.

   procedure Error_Msg_Ada_2020_Feature (Feature : String; Loc : Source_Ptr);
   --  Analogous to Error_Msg_Ada_2012_Feature

   procedure dmsg (Id : Error_Msg_Id) renames Erroutc.dmsg;
   --  Debugging routine to dump an error message

   ------------------------------------
   -- SPARK Error Output Subprograms --
   ------------------------------------

   --  The following routines are intended to report semantic errors in SPARK
   --  constructs subject to aspect/pragma SPARK_Mode. Note that syntax errors
   --  must be reported using the Error_Msg_XXX routines. This allows for the
   --  partial analysis of SPARK features when they are disabled via SPARK_Mode
   --  set to "off".

   procedure SPARK_Msg_N (Msg : String; N : Node_Or_Entity_Id);
   pragma Inline (SPARK_Msg_N);
   --  Same as Error_Msg_N, but the error is suppressed if SPARK_Mode is Off.
   --  The routine is inlined because it acts as a simple wrapper.

   procedure SPARK_Msg_NE
     (Msg : String;
      N   : Node_Or_Entity_Id;
      E   : Node_Or_Entity_Id);
   pragma Inline (SPARK_Msg_NE);
   --  Same as Error_Msg_NE, but the error is suppressed if SPARK_Mode is Off.
   --  The routine is inlined because it acts as a simple wrapper.

   ------------------------------------------
   -- Utility Interface for Casing Control --
   ------------------------------------------

   procedure Adjust_Name_Case
     (Buf : in out Bounded_String;
      Loc : Source_Ptr);
   --  Given a name stored in Buf, set proper casing. Loc is an associated
   --  source position, and if we can find a match between the name in Buf and
   --  the name at that source location, we copy the casing from the source,
   --  otherwise we set appropriate default casing.

   procedure Adjust_Name_Case (Loc : Source_Ptr);
   --  Uses Buf => Global_Name_Buffer. There are no calls to this in the
   --  compiler, but it is called in SPARK 2014.

   procedure Set_Identifier_Casing
     (Identifier_Name : System.Address;
      File_Name       : System.Address);
   pragma Convention (C, Set_Identifier_Casing);
   --  This subprogram can be used by the back end for the purposes of
   --  concocting error messages that are not output via Errout, e.g.
   --  the messages generated by the gcc back end.
   --
   --  The identifier is a null terminated string that represents the name of
   --  an identifier appearing in the source program. File_Name is a null
   --  terminated string giving the corresponding file name for the identifier
   --  as obtained from the front end by the use of Full_Debug_Name to the
   --  source file referenced by the corresponding source location value. On
   --  return, the name is in Name_Buffer, null terminated with Name_Len set.
   --  This name is the identifier name as passed, cased according to the
   --  default identifier casing for the given file.

   --  WARNING: There is a matching C declaration of this subprogram in fe.h

   function Is_Size_Too_Small_Message (S : String) return Boolean;
   Size_Too_Small_Message : constant String :=
     "size for& too small, minimum allowed is ^";
   --  This message is printed in Freeze and Sem_Ch13. We also test for it in
   --  the body of this package (see Special_Msg_Delete).
   --  Function Is_Size_Too_Small_Message tests for it by testing a prefix.
   --  The function and constant should be kept in synch.

end Errout;

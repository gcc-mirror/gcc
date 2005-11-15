------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              E R R U T I L                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2002-2005, Free Software Foundation, Inc.         --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains routines to output error messages and the
--  corresponding instantiation of Styleg, suitable to instantiate Scng.

--  It is not dependent on the GNAT tree packages (Atree, Sinfo, ...)

--  It uses the same global variables as Errout, located in package
--  Err_Vars. Like Errout, it also uses the common variables and routines
--  in package Erroutc.

--  This package is used by the preprocessor (gprep.adb) and the project
--  manager (prj-err.ads).

with Styleg;
with Types; use Types;

package Errutil is

   ---------------------------------------------------------
   -- Error Message Text and Message Insertion Characters --
   ---------------------------------------------------------

   --  Error message text strings are composed of lower case letters, digits
   --  and the special characters space, comma, period, colon and semicolon,
   --  apostrophe and parentheses. Special insertion characters can also
   --  appear which cause the error message circuit to modify the given
   --  string as follows:

   --    Ignored insertion characters: the following characters, used as
   --      insertion characters by Errout are ignored: '$', '&', and '}'.
   --      If present in an error message, they are not output and are not
   --      replaced by any text.

   --    Insertion character % (Percent: insert name from Names table)
   --      The character % is replaced by the text for the name specified by
   --      the Name_Id value stored in Error_Msg_Name_1. A blank precedes
   --      the name if it is preceded by a non-blank character other than a
   --      left parenthesis. The name is enclosed in quotes unless manual
   --      quotation mode is set. If the Name_Id is set to No_Name, then
   --      no insertion occurs; if the Name_Id is set to Error_Name, then
   --      the string <error> is inserted. A second and third % may appear
   --      in a single message, similarly replaced by the names which are
   --      specified by the Name_Id values stored in Error_Msg_Name_2 and
   --      Error_Msg_Name_3. The names are decoded and cased according to
   --      the current identifier casing mode.

   --    Insertion character { (Left brace: insert literally from names table)
   --      The character { is treated similarly to %, except that the
   --      name is output literally as stored in the names table without
   --      adjusting the casing. This can be used for file names and in
   --      other situations where the name string is to be output unchanged.

   --    Insertion character * (Asterisk, insert reserved word name)
   --      The insertion character * is treated exactly like % except that
   --      the resulting name is cased according to the default conventions
   --      for reserved words (see package Scans).

   --    Insertion character # (Pound: insert line number reference)
   --      The character # is replaced by the string indicating the source
   --      position stored in Error_Msg_Sloc. There are two cases:
   --
   --        for locations in current file:  at line nnn:ccc
   --        for locations in other files:   at filename:nnn:ccc
   --
   --      By convention, the # insertion character is only used at the end
   --      of an error message, so the above strings only appear as the last
   --      characters of an error message.

   --    Insertion character @ (At: insert column number reference)
   --      The character @ is replaced by null if the RM_Column_Check mode is
   --      off (False). If the switch is on (True), then @ is replaced by the
   --      text string " in column nnn" where nnn is the decimal representation
   --      of the column number stored in Error_Msg_Col plus one (the plus one
   --      is because the number is stored 0-origin and displayed 1-origin).

   --    Insertion character ^ (Carret: insert integer value)
   --      The character ^ is replaced by the decimal conversion of the Uint
   --      value stored in Error_Msg_Uint_1, with a possible leading minus.
   --      A second ^ may occur in the message, in which case it is replaced
   --      by the decimal conversion of the Uint value in Error_Msg_Uint_2.

   --    Insertion character ! (Exclamation: unconditional message)
   --      The character ! appearing as the last character of a message makes
   --      the message unconditional which means that it is output even if it
   --      would normally be suppressed.

   --    Insertion character ? (Question: warning message)
   --      The character ? appearing anywhere in a message makes the message
   --      a warning instead of a normal error message, and the text of the
   --      message will be preceded by "Warning:" instead of "Error:" The
   --      handling of warnings if further controlled by the Warning_Mode
   --      option (-w switch), see package Opt for further details, and
   --      also by the current setting from pragma Warnings. This pragma
   --      applies only to warnings issued from the semantic phase (not
   --      the parser), but currently all relevant warnings are posted
   --      by the semantic phase anyway. Messages starting with (style)
   --      are also treated as warning messages.

   --    Insertion character A-Z (Upper case letter: Ada reserved word)
   --      If two or more upper case letters appear in the message, they are
   --      taken as an Ada reserved word, and are converted to the default
   --      case for reserved words (see Scans package spec). Surrounding
   --      quotes are added unless manual quotation mode is currently set.

   --    Insertion character ` (Backquote: set manual quotation mode)
   --      The backquote character always appears in pairs. Each backquote
   --      of the pair is replaced by a double quote character. In addition,
   --      Any reserved keywords, or name insertions between these backquotes
   --      are not surrounded by the usual automatic double quotes. See the
   --      section below on manual quotation mode for further details.

   --    Insertion character ' (Quote: literal character)
   --      Precedes a character which is placed literally into the message.
   --      Used to insert characters into messages that are one of the
   --      insertion characters defined here.

   --    Insertion character \ (Backslash: continuation message)
   --      Indicates that the message is a continuation of a message
   --      previously posted. This is used to ensure that such groups
   --      of messages are treated as a unit. The \ character must be
   --      the first character of the message text.

   -----------------------------------------------------
   -- Format of Messages and Manual Quotation Control --
   -----------------------------------------------------

   --  Messages are generally all in lower case, except for inserted names
   --  and appear in one of the following two forms:

   --    error: text
   --    warning: text

   --  The prefixes error and warning are supplied automatically (depending
   --  on the use of the ? insertion character), and the call to the error
   --  message routine supplies the text. The "error: " prefix is omitted
   --  in brief error message formats.

   --  Reserved keywords in the message are in the default keyword case
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

   ------------------------------
   -- Error Output Subprograms --
   ------------------------------

   procedure Initialize;
   --  Initializes for output of error messages. Must be called for each
   --  file before using any of the other routines in the package.

   procedure Finalize (Source_Type : String := "project");
   --  Finalize processing of error messages for one file and output message
   --  indicating the number of detected errors.
   --  Source_Type is used in verbose mode to indicate the type of the source
   --  being parsed (project file, definition file or input file for the
   --  preprocessor).

   procedure Error_Msg (Msg : String; Flag_Location : Source_Ptr);
   --  Output a message at specified location

   procedure Error_Msg_S (Msg : String);
   --  Output a message at current scan pointer location

   procedure Error_Msg_SC (Msg : String);
   --  Output a message at the start of the current token, unless we are at
   --  the end of file, in which case we always output the message after the
   --  last real token in the file.

   procedure Error_Msg_SP (Msg : String);
   --  Output a message at the start of the previous token

   procedure Set_Ignore_Errors (To : Boolean);
   --  Indicate, when To = True, that all reported errors should
   --  be ignored. By default reported errors are not ignored.

   package Style is new Styleg
     (Error_Msg    => Error_Msg,
      Error_Msg_S  => Error_Msg_S,
      Error_Msg_SC => Error_Msg_SC,
      Error_Msg_SP => Error_Msg_SP);
   --  Instantiation of the generic style package, suitable for an
   --  instantiation of Scng.

end Errutil;

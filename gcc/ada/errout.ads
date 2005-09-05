------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               E R R O U T                                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2005 Free Software Foundation, Inc.          --
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

--  This package contains the routines to output error messages. They are
--  basically system independent, however in some environments, e.g. when the
--  parser is embedded into an editor, it may be appropriate to replace the
--  implementation of this package.

with Err_Vars;
with Erroutc;
with Table;
with Types; use Types;
with Uintp; use Uintp;

with System;

package Errout is

   Serious_Errors_Detected : Nat renames Err_Vars.Serious_Errors_Detected;
   --  This is a count of errors that are serious enough to stop expansion,
   --  and hence to prevent generation of an object file even if the switch
   --  -gnatQ is set.

   Total_Errors_Detected : Nat renames Err_Vars.Total_Errors_Detected;
   --  Number of errors detected so far. Includes count of serious errors and
   --  non-serious errors, so this value is always greater than or equal to
   --  the Serious_Errors_Detected value.

   Warnings_Detected : Nat renames Err_Vars.Warnings_Detected;
   --  Number of warnings detected

   Configurable_Run_Time_Violations : Nat := 0;
   --  Count of configurable run time violations so far. This is used to
   --  suppress certain cascaded error messages when we know that we may not
   --  have fully expanded some items, due to high integrity violations (i.e.
   --  the use of constructs not permitted by the library in use, or improper
   --  constructs in No_Run_Time mode).

   type Compiler_State_Type is (Parsing, Analyzing);
   Compiler_State : Compiler_State_Type;
   --  Indicates current state of compilation. This is put in the Errout spec
   --  because it affects the action of the error message handling. In
   --  particular, an attempt is made by Errout to suppress cascaded error
   --  messages in Parsing mode, but not in the other modes.

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

   --  This normal suppression action may be overridden in cases 2-5 (but not
   --  in case 1) by setting All_Errors mode, or by setting the special
   --  unconditional message insertion character (!) at the end of the message
   --  text as described below.

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
   --      casing mode.

   --    Insertion character $ (Dollar: insert unit name from Names table)
   --      The character $ is treated similarly to %, except that the name is
   --      obtained from the Unit_Name_Type value in Error_Msg_Unit_1 and
   --      Error_Msg_Unit_2, as provided by Get_Unit_Name_String in package
   --      Uname. Note that this name includes the postfix (spec) or (body)
   --      strings. If this postfix is not required, use the normal %
   --      insertion for the unit name.

   --    Insertion character { (Left brace: insert literally from names table)
   --      The character { is treated similarly to %, except that the name is
   --      output literally as stored in the names table without adjusting the
   --      casing. This can be used for file names and in other situations
   --      where the name string is to be output unchanged.

   --    Insertion character * (Asterisk, insert reserved word name)
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
   --      characters of an error message.

   --    Insertion character } (Right brace: insert type reference)
   --      The character } is replaced by a string describing the type
   --      referenced by the entity whose Id is stored in Error_Msg_Node_1.
   --      the string gives the name or description of the type, and also
   --      where appropriate the location of its declaration. Special cases
   --      like "some integer type" are handled appropriately. Only one } is
   --      allowed in a message, since there is not enough room for two (the
   --      insertion can be quite long, including a file name) In addition, if
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

   --    Insertion character ^ (Carret: insert integer value)
   --      The character ^ is replaced by the decimal conversion of the Uint
   --      value stored in Error_Msg_Uint_1, with a possible leading minus.
   --      A second ^ may occur in the message, in which case it is replaced
   --      by the decimal conversion of the Uint value in Error_Msg_Uint_2.

   --    Insertion character > (Right bracket, run time name)
   --      The character > is replaced by a string of the form (name) if
   --      Targparm scanned out a Run_Time_Name (see package Targparm for
   --      details). The name is enclosed in parentheses and output in mixed
   --      case mode (upper case after any space in the name). If no run time
   --      name is defined, this insertion character has no effect.

   --    Insertion character ! (Exclamation: unconditional message)
   --      The character ! appearing as the last character of a message makes
   --      the message unconditional which means that it is output even if it
   --      would normally be suppressed. See section above for a description
   --      of the cases in which messages are normally suppressed.

   --    Insertion character ? (Question: warning message)
   --      The character ? appearing anywhere in a message makes the message
   --      a warning instead of a normal error message, and the text of the
   --      message will be preceded by "Warning:" instead of "Error:" The
   --      handling of warnings if further controlled by the Warning_Mode
   --      option (-w switch), see package Opt for further details, and also
   --      by the current setting from pragma Warnings. This pragma applies
   --      only to warnings issued from the semantic phase (not the parser),
   --      but currently all relevant warnings are posted by the semantic
   --      phase anyway. Messages starting with (style) are also treated as
   --      warning messages.

   --    Insertion character < (Less Than: conditional warning message)
   --      The character < appearing anywhere in a message is used for a
   --      conditional error message. If Error_Msg_Warn is True, then the
   --      effect is the same as ? described above. If Error_Msg_Warn is
   --      False, then there is no effect.

   --    Insertion character A-Z (Upper case letter: Ada reserved word)
   --      If two or more upper case letters appear in the message, they are
   --      taken as an Ada reserved word, and are converted to the default
   --      case for reserved words (see Scans package spec). Surrounding
   --      quotes are added unless manual quotation mode is currently set.

   --    Insertion character ` (Backquote: set manual quotation mode)
   --      The backquote character always appears in pairs. Each backquote of
   --      the pair is replaced by a double quote character. In addition, Any
   --      reserved keywords, or name insertions between these backquotes are
   --      not surrounded by the usual automatic double quotes. See the
   --      section below on manual quotation mode for further details.

   --    Insertion character ' (Quote: literal character)
   --      Precedes a character which is placed literally into the message.
   --      Used to insert characters into messages that are one of the
   --      insertion characters defined here. Also useful in inserting
   --      sequences of upper case letters (e.g. RM) which are not to be
   --      treated as keywords.

   --    Insertion character \ (Backslash: continuation message)
   --      Indicates that the message is a continuation of a message
   --      previously posted. This is used to ensure that such groups of
   --      messages are treated as a unit. The \ character must be the first
   --      character of the message text.

   --    Insertion character | (vertical bar, non-serious error)
   --      By default, error messages (other than warning messages) are
   --      considered to be fatal error messages which prevent expansion or
   --      generation of code in the presence of the -gnatQ switch. If the
   --      insertion character | appears, the message is considered to be
   --      non-serious, and does not cause Serious_Errors_Detected to be
   --      incremented (so expansion is not prevented by such a msg).

   ----------------------------------------
   -- Specialization of Messages for VMS --
   ----------------------------------------

   --  Some messages mention gcc-style switch names. When using an OpenVMS
   --  host, such switch names must be converted to their corresponding VMS
   --  qualifer. The following table controls this translation. In each case
   --  the original message must contain the string "-xxx switch", where xxx
   --  is the Gname? entry from below, and this string will be replaced by
   --  "/yyy qualifier", where yyy is the corresponding Vname? entry.

   Gname1 : aliased constant String := "fno-strict-aliasing";
   Vname1 : aliased constant String := "OPTIMIZE=NO_STRICT_ALIASING";

   Gname2 : aliased constant String := "gnatX";
   Vname2 : aliased constant String := "EXTENSIONS_ALLOWED";

   Gname3 : aliased constant String := "gnatW";
   Vname3 : aliased constant String := "WIDE_CHARACTER_ENCODING";

   Gname4 : aliased constant String := "gnatf";
   Vname4 : aliased constant String := "REPORT_ERRORS=FULL";

   Gname5 : aliased constant String := "gnat05";
   Vname5 : aliased constant String := "05";

   type Cstring_Ptr is access constant String;

   Gnames : array (Nat range <>) of Cstring_Ptr :=
              (Gname1'Access,
               Gname2'Access,
               Gname3'Access,
               Gname4'Access,
               Gname5'Access);

   Vnames : array (Nat range <>) of Cstring_Ptr :=
              (Vname1'Access,
               Vname2'Access,
               Vname3'Access,
               Vname4'Access,
               Vname5'Access);

   -----------------------------------------------------
   -- Global Values Used for Error Message Insertions --
   -----------------------------------------------------

   --  The following global variables are essentially additional parameters
   --  passed to the error message routine for insertion sequences described
   --  above. The reason these are passed globally is that the insertion
   --  mechanism is essentially an untyped one in which the appropriate
   --  variables are set dependingon the specific insertion characters used.

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

   Error_Msg_Unit_1 : Name_Id renames Err_Vars.Error_Msg_Unit_1;
   Error_Msg_Unit_2 : Name_Id renames Err_Vars.Error_Msg_Unit_2;
   --  Name_Id values for $ insertion characters in message

   Error_Msg_Node_1 : Node_Id renames Err_Vars.Error_Msg_Node_1;
   Error_Msg_Node_2 : Node_Id renames Err_Vars.Error_Msg_Node_2;
   --  Node_Id values for & insertion characters in message

   Error_Msg_Qual_Level : Int renames Err_Vars.Error_Msg_Qual_Level;
   --  Number of levels of qualification required for type name (see the
   --  description of the } insertion character. Note that this value does
   --  note get reset by any Error_Msg call, so the caller is responsible
   --  for resetting it.

   Error_Msg_Warn : Boolean renames Err_Vars.Error_Msg_Warn;
   --  Used if current message contains a < insertion character to indicate
   --  if the current message is a warning message.

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

   ------------------------------
   -- Error Output Subprograms --
   ------------------------------

   procedure Initialize;
   --  Initializes for output of error messages. Must be called for each
   --  source file before using any of the other routines in the package.

   procedure Finalize;
   --  Finalize processing of error messages for one file and output message
   --  indicating the number of detected errors.

   procedure Error_Msg (Msg : String; Flag_Location : Source_Ptr);
   --  Output a message at specified location. Can be called from the parser
   --  or the semantic analyzer.

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
   --  warning and warnings and N is an entity node for which warnings are
   --  suppressed.

   procedure Error_Msg_F (Msg : String; N : Node_Id);
   --  Similar to Error_Msg_N except that the message is placed on the
   --  first node of the construct N (First_Node (N)).

   procedure Error_Msg_NE
     (Msg : String;
      N   : Node_Or_Entity_Id;
      E   : Node_Or_Entity_Id);
   --  Output a message at the Sloc of the given node N, with an insertion of
   --  the name from the given entity node E. This is used by the semantic
   --  routines, where this is a common error message situation. The Msg text
   --  will contain a & or } as usual to mark the insertion point. This
   --  routine can be called from the parser or the analyzer.

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
   --  Exactly the same as Error_Msg_NE, except that the flag is placed at
   --  the specified Flag_Location instead of at Sloc (N).

   procedure Error_Msg_NW
     (Eflag : Boolean;
      Msg   : String;
      N     : Node_Or_Entity_Id);
   --  This routine is used for posting a message conditionally. The message
   --  is posted (with the same effect as Error_Msg_N (Msg, N) if and only
   --  if Eflag is True and if the node N is within the main extended source
   --  unit and comes from source. Typically this is a warning mode flag.

   procedure Change_Error_Text (Error_Id : Error_Msg_Id; New_Msg : String);
   --  The error message text of the message identified by Id is replaced by
   --  the given text. This text may contain insertion characters in the
   --  usual manner, and need not be the same length as the original text.

   function First_Node (C : Node_Id) return Node_Id;
   --  Given a construct C, finds the first node in the construct, i.e. the
   --  one with the lowest Sloc value. This is useful in placing error msgs.

   function First_Sloc (N : Node_Id) return Source_Ptr;
   --  Given the node for an expression, return a source pointer value that
   --  points to the start of the first token in the expression. In the case
   --  where the expression is parenthesized, an attempt is made to include
   --  the parentheses (i.e. to return the location of the initial paren).

   procedure Purge_Messages (From : Source_Ptr; To : Source_Ptr)
     renames Erroutc.Purge_Messages;
   --  All error messages whose location is in the range From .. To (not
   --  including the end points) will be deleted from the error listing.

   procedure Remove_Warning_Messages (N : Node_Id);
   --  Remove any warning messages corresponding to the Sloc of N or any
   --  of its descendent nodes. No effect if no such warnings.

   procedure Remove_Warning_Messages (L : List_Id);
   --  Remove warnings on all elements of a list

   procedure Set_Ignore_Errors (To : Boolean);
   --  Following a call to this procedure with To=True, all error calls are
   --  ignored. A call with To=False restores the default treatment in which
   --  error calls are treated as usual (and as described in this spec).

   procedure Set_Warnings_Mode_Off (Loc : Source_Ptr)
     renames Erroutc.Set_Warnings_Mode_Off;
   --  Called in response to a pragma Warnings (Off) to record the source
   --  location from which warnings are to be turned off.

   procedure Set_Warnings_Mode_On (Loc : Source_Ptr)
     renames Erroutc.Set_Warnings_Mode_On;
   --  Called in response to a pragma Warnings (On) to record the source
   --  location from which warnings are to be turned back on.

   function Compilation_Errors return Boolean
     renames Erroutc.Compilation_Errors;
   --  Returns true if errors have been detected, or warnings in -gnatwe
   --  (treat warnings as errors) mode.

   procedure Error_Msg_CRT (Feature : String; N : Node_Id);
   --  Posts a non-fatal message on node N saying that the feature identified
   --  by the Feature argument is not supported in either configurable
   --  run-time mode or no run-time mode (as appropriate). In the former case,
   --  the name of the library is output if available.

   procedure dmsg (Id : Error_Msg_Id) renames Erroutc.dmsg;
   --  Debugging routine to dump an error message

   ------------------------------------
   -- Utility Interface for Back End --
   ------------------------------------

   --  The following subprograms can be used by the back end for the purposes
   --  of concocting error messages that are not output via Errout, e.g. the
   --  messages generated by the gcc back end.

   procedure Set_Identifier_Casing
     (Identifier_Name : System.Address;
      File_Name       : System.Address);
   --  The identifier is a null terminated string that represents the name of
   --  an identifier appearing in the source program. File_Name is a null
   --  terminated string giving the corresponding file name for the identifier
   --  as obtained from the front end by the use of Full_Debug_Name to the
   --  source file referenced by the corresponding source location value. On
   --  return, the name is in Name_Buffer, null terminated with Name_Len set.
   --  This name is the identifier name as passed, cased according to the
   --  default identifier casing for the given file.

end Errout;

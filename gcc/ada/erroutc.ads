------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              E R R O U T C                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2022, Free Software Foundation, Inc.         --
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

--  This package contains global variables and routines common to error
--  reporting packages, including Errout and Prj.Err.

with Table;
with Types; use Types;

package Erroutc is

   Class_Flag : Boolean := False;
   --  This flag is set True when outputting a reference to a class-wide
   --  type, and is used by Add_Class to insert 'Class at the proper point

   Continuation : Boolean := False;
   --  Indicates if current message is a continuation. Initialized from the
   --  Msg_Cont parameter in Error_Msg_Internal and then set True if a \
   --  insertion character is encountered.

   Continuation_New_Line : Boolean := False;
   --  Indicates if current message was a continuation line marked with \\ to
   --  force a new line. Set True if \\ encountered.

   Flag_Source : Source_File_Index;
   --  Source file index for source file where error is being posted

   Has_Double_Exclam : Boolean := False;
   --  Set true to indicate that the current message contains the insertion
   --  sequence !! (force warnings even in non-main unit source files).

   Has_Insertion_Line : Boolean := False;
   --  Set True to indicate that the current message contains the insertion
   --  character # (insert line number reference).

   Is_Compile_Time_Msg : Boolean := False;
   --  Set true to indicate that the current message originates from a
   --  Compile_Time_Warning or Compile_Time_Error pragma.

   Is_Serious_Error : Boolean := False;
   --  Set True for a serious error (i.e. any message that is not a warning
   --  or style message, and that does not contain a | insertion character).

   Is_Unconditional_Msg : Boolean := False;
   --  Set True to indicate that the current message contains the insertion
   --  character ! and is thus to be treated as an unconditional message.

   Is_Warning_Msg : Boolean := False;
   --  Set True to indicate if current message is warning message (contains ?
   --  or contains < and Error_Msg_Warn is True).

   Is_Runtime_Raise : Boolean := False;
   --  Set to True to indicate that the current message is a warning about a
   --  constraint error that will be raised at runtime (contains [ and switch
   --  -gnatwE was given).

   Is_Info_Msg : Boolean := False;
   --  Set True to indicate that the current message starts with the characters
   --  "info: " and is to be treated as an information message. This string
   --  will be prepended to the message and all its continuations.

   Is_Check_Msg : Boolean := False;
   --  Set True to indicate that the current message starts with one of
   --  "high: ", "medium: ", "low: " and is to be treated as a check message.

   Warning_Msg_Char : String (1 .. 2);
   --  Warning switch, valid only if Is_Warning_Msg is True
   --    "  "      -- ?   or <   appeared on its own in message
   --    "? "      -- ??  or <<  appeared in message
   --    "x "      -- ?x? or <x< appeared in message
   --              -- (x = a .. z | A .. Z | * | $)
   --    ".x"      -- ?.x? appeared in message (x = a .. z | A .. Z)
   --    "_x"      -- ?_x? appeared in message (x = a .. z | A .. Z)
   --  In the case of the < sequences, this is set only if the message is
   --  actually a warning, i.e. if Error_Msg_Warn is True

   Is_Style_Msg : Boolean := False;
   --  Set True to indicate if the current message is a style message
   --  (i.e. a message whose text starts with the characters "(style)").

   Kill_Message : Boolean := False;
   --  A flag used to kill weird messages (e.g. those containing uninterpreted
   --  implicit type references) if we have already seen at least one message
   --  already. The idea is that we hope the weird message is a junk cascaded
   --  message that should be suppressed.

   Last_Killed : Boolean := False;
   --  Set True if the most recently posted non-continuation message was
   --  killed. This is used to determine the processing of any continuation
   --  messages that follow.

   List_Pragmas_Index : Int := 0;
   --  Index into List_Pragmas table

   List_Pragmas_Mode : Boolean := False;
   --  Starts True, gets set False by pragma List (Off), True by List (On)

   Manual_Quote_Mode : Boolean := False;
   --  Set True in manual quotation mode

   Max_Msg_Length : constant := 1024 + 2 * Int (Column_Number'Last);
   --  Maximum length of error message. The addition of 2 * Column_Number'Last
   --  ensures that two insertion tokens of maximum length can be accommodated.
   --  The value of 1024 is an arbitrary value that should be more than long
   --  enough to accommodate any reasonable message (and for that matter, some
   --  pretty unreasonable messages).

   Msg_Buffer : String (1 .. Max_Msg_Length);
   --  Buffer used to prepare error messages

   Msglen : Integer := 0;
   --  Number of characters currently stored in the message buffer

   Suppress_Message : Boolean;
   --  A flag used to suppress certain obviously redundant messages (i.e.
   --  those referring to a node whose type is Any_Type). This suppression
   --  is effective only if All_Errors_Mode is off.

   Suppress_Instance_Location : Boolean := False;
   --  Normally, if a # location in a message references a location within
   --  a generic template, then a note is added giving the location of the
   --  instantiation. If this variable is set True, then this note is not
   --  output. This is used for internal processing for the case of an
   --  illegal instantiation. See Error_Msg routine for further details.

   type Subprogram_Name_Type is access function (N : Node_Id) return String;
   Subprogram_Name_Ptr : Subprogram_Name_Type;
   --  Indirect call to Sem_Util.Subprogram_Name to break circular
   --  dependency with the static elaboration model.

   ----------------------------
   -- Message ID Definitions --
   ----------------------------

   type Error_Msg_Id is new Int;
   --  A type used to represent specific error messages. Used by the clients
   --  of this package only in the context of the Get_Error_Id and
   --  Change_Error_Text subprograms.

   No_Error_Msg : constant Error_Msg_Id := 0;
   --  A constant which is different from any value returned by Get_Error_Id.
   --  Typically used by a client to indicate absence of a saved Id value.

   Cur_Msg : Error_Msg_Id := No_Error_Msg;
   --  Id of most recently posted error message

   function Get_Msg_Id return Error_Msg_Id;
   --  Returns the Id of the message most recently posted using one of the
   --  Error_Msg routines.

   function Get_Location (E : Error_Msg_Id) return Source_Ptr;
   --  Returns the flag location of the error message with the given id E

   -----------------------------------
   -- Error Message Data Structures --
   -----------------------------------

   --  The error messages are stored as a linked list of error message objects
   --  sorted into ascending order by the source location (Sloc). Each object
   --  records the text of the message and its source location.

   --  The following record type and table are used to represent error
   --  messages, with one entry in the table being allocated for each message.

   type Error_Msg_Object is record
      Text : String_Ptr;
      --  Text of error message, fully expanded with all insertions

      Next : Error_Msg_Id;
      --  Pointer to next message in error chain. A value of No_Error_Msg
      --  indicates the end of the chain.

      Prev : Error_Msg_Id;
      --  Pointer to previous message in error chain. Only set during the
      --  Finalize procedure. A value of No_Error_Msg indicates the first
      --  message in the chain.

      Sfile : Source_File_Index;
      --  Source table index of source file. In the case of an error that
      --  refers to a template, always references the original template
      --  not an instantiation copy.

      Sptr : Source_Span;
      --  Flag pointer. In the case of an error that refers to a template,
      --  always references the original template, not an instantiation copy.
      --  This value is the actual place in the source that the error message
      --  will be posted. Note that an error placed on an instantiation will
      --  have Sptr pointing to the instantiation point.

      Optr : Source_Span;
      --  Flag location used in the call to post the error. This is the same as
      --  Sptr, except when an error is posted on a particular instantiation of
      --  a generic. In such a case, Sptr will point to the original source
      --  location of the instantiation itself, but Optr will point to the
      --  template location (more accurately to the template copy in the
      --  instantiation copy corresponding to the instantiation referenced by
      --  Sptr).

      Insertion_Sloc : Source_Ptr;
      --  Location in message for insertion character # when used

      Line : Physical_Line_Number;
      --  Line number for error message

      Col : Column_Number;
      --  Column number for error message

      Compile_Time_Pragma : Boolean;
      --  True if the message originates from a Compile_Time_Warning or
      --  Compile_Time_Error pragma

      Warn : Boolean;
      --  True if warning message

      Info : Boolean;
      --  True if info message

      Check : Boolean;
      --  True if check message

      Warn_Err : Boolean;
      --  True if this is a warning message which is to be treated as an error
      --  as a result of a match with a Warning_As_Error pragma.

      Warn_Runtime_Raise : Boolean;
      --  True if this a warning about a constraint error that will be raised
      --  at runtime.

      Warn_Chr : String (1 .. 2);
      --  See Warning_Msg_Char

      Style : Boolean;
      --  True if style message (starts with "(style)")

      Serious : Boolean;
      --  True if serious error message (not a warning and no | character)

      Uncond : Boolean;
      --  True if unconditional message (i.e. insertion character ! appeared)

      Msg_Cont : Boolean;
      --  This is used for logical messages that are composed of multiple
      --  individual messages. For messages that are not part of such a
      --  group, or that are the first message in such a group. Msg_Cont
      --  is set to False. For subsequent messages in a group, Msg_Cont
      --  is set to True. This is used to make sure that such a group of
      --  messages is either suppressed or retained as a group (e.g. in
      --  the circuit that deletes identical messages).

      Deleted : Boolean;
      --  If this flag is set, the message is not printed. This is used
      --  in the circuit for deleting duplicate/redundant error messages.

      Node : Node_Id;
      --  If set, points to the node relevant for this message which will be
      --  used to compute the enclosing subprogram name if
      --  Opt.Include_Subprogram_In_Messages is set.
   end record;

   package Errors is new Table.Table (
     Table_Component_Type => Error_Msg_Object,
     Table_Index_Type     => Error_Msg_Id,
     Table_Low_Bound      => 1,
     Table_Initial        => 200,
     Table_Increment      => 200,
     Table_Name           => "Error");

   First_Error_Msg : Error_Msg_Id;
   --  The list of error messages, i.e. the first entry on the list of error
   --  messages. This is not the same as the physically first entry in the
   --  error message table, since messages are not always inserted in sequence.

   Last_Error_Msg : Error_Msg_Id;
   --  The last entry on the list of error messages. Note: this is not the same
   --  as the physically last entry in the error message table, since messages
   --  are not always inserted in sequence.

   --------------------------
   -- Warning Mode Control --
   --------------------------

   --  Pragma Warnings allows warnings to be turned off for a specified region
   --  of code, and the following tables are the data structures used to keep
   --  track of these regions.

   --  The first table is used for the basic command line control, and for the
   --  forms of Warning with a single ON or OFF parameter.

   --  It contains pairs of source locations, the first being the start
   --  location for a warnings off region, and the second being the end
   --  location. When a pragma Warnings (Off) is encountered, a new entry is
   --  established extending from the location of the pragma to the end of the
   --  current source file. A subsequent pragma Warnings (On) adjusts the end
   --  point of this entry appropriately.

   --  If all warnings are suppressed by command switch, then there is a dummy
   --  entry (put there by Errout.Initialize) at the start of the table which
   --  covers all possible Source_Ptr values. Note that the source pointer
   --  values in this table always reference the original template, not an
   --  instantiation copy, in the generic case.

   --  Reason is the reason from the pragma Warnings (Off,..) or the null
   --  string if no reason parameter is given.

   type Warnings_Entry is record
      Start  : Source_Ptr;
      Stop   : Source_Ptr;
      Reason : String_Id;
   end record;

   package Warnings is new Table.Table (
     Table_Component_Type => Warnings_Entry,
     Table_Index_Type     => Natural,
     Table_Low_Bound      => 1,
     Table_Initial        => 100,
     Table_Increment      => 200,
     Table_Name           => "Warnings");

   --  The second table is used for the specific forms of the pragma, where
   --  the first argument is ON or OFF, and the second parameter is a string
   --  which is the pattern to match for suppressing a warning.

   type Specific_Warning_Entry is record
      Start : Source_Ptr;
      Stop  : Source_Ptr;
      --  Starting and ending source pointers for the range. These are always
      --  from the same source file.

      Reason : String_Id;
      --  Reason string from pragma Warnings, or null string if none

      Msg : String_Ptr;
      --  Message from pragma Warnings (Off, string)

      Open : Boolean;
      --  Set to True if OFF has been encountered with no matching ON

      Used : Boolean;
      --  Set to True if entry has been used to suppress a warning

      Config : Boolean;
      --  True if pragma is configuration pragma (in which case no matching Off
      --  pragma is required, and it is not required that a specific warning be
      --  suppressed).
   end record;

   package Specific_Warnings is new Table.Table (
     Table_Component_Type => Specific_Warning_Entry,
     Table_Index_Type     => Natural,
     Table_Low_Bound      => 1,
     Table_Initial        => 100,
     Table_Increment      => 200,
     Table_Name           => "Specific_Warnings");

   --  Note on handling configuration case versus specific case. A complication
   --  arises from this example:

   --     pragma Warnings (Off, "not referenced*");
   --     procedure Mumble (X : Integer) is
   --     pragma Warnings (On, "not referenced*");
   --     begin
   --        null;
   --     end Mumble;

   --  The trouble is that the first pragma is technically a configuration
   --  pragma, and yet it is clearly being used in the context of thinking of
   --  it as a specific case. To deal with this, what we do is that the On
   --  entry can match a configuration pragma from the same file, and if we
   --  find such an On entry, we cancel the indication of it being the
   --  configuration case. This seems to handle all cases we run into ok.

   -------------------
   -- Color Control --
   -------------------

   Use_SGR_Control : Boolean := False;
   --  Set to True for enabling colored output. This should only be done when
   --  outputting messages to a terminal that supports it.

   --  Colors in messages output to a terminal are controlled using SGR
   --  (Select Graphic Rendition).

   Color_Separator  : constant String := ";";
   Color_None       : constant String := "00";
   Color_Bold       : constant String := "01";
   Color_Underscore : constant String := "04";
   Color_Blink      : constant String := "05";
   Color_Reverse    : constant String := "07";
   Color_Fg_Black   : constant String := "30";
   Color_Fg_Red     : constant String := "31";
   Color_Fg_Green   : constant String := "32";
   Color_Fg_Yellow  : constant String := "33";
   Color_Fg_Blue    : constant String := "34";
   Color_Fg_Magenta : constant String := "35";
   Color_Fg_Cyan    : constant String := "36";
   Color_Fg_White   : constant String := "37";
   Color_Bg_Black   : constant String := "40";
   Color_Bg_Red     : constant String := "41";
   Color_Bg_Green   : constant String := "42";
   Color_Bg_Yellow  : constant String := "43";
   Color_Bg_Blue    : constant String := "44";
   Color_Bg_Magenta : constant String := "45";
   Color_Bg_Cyan    : constant String := "46";
   Color_Bg_White   : constant String := "47";

   SGR_Start        : constant String := ASCII.ESC & "[";
   SGR_End          : constant String := "m" & ASCII.ESC & "[K";

   function SGR_Seq (Str : String) return String is
     (if Use_SGR_Control then SGR_Start & Str & SGR_End else "");
   --  Return the SGR control string for the commands in Str. It returns the
   --  empty string if Use_SGR_Control is False, so that we can insert this
   --  string unconditionally.

   function SGR_Reset return String is (SGR_Seq (""));
   --  This ends the current section of colored output

   --  We're using the same colors as gcc/g++ for errors/warnings/notes/locus.
   --  More colors are defined in gcc/g++ for other features of diagnostic
   --  messages (e.g. inline types, fixit) and could be used in GNAT in the
   --  future. The following functions start a section of colored output.

   function SGR_Error return String is
     (SGR_Seq (Color_Bold & Color_Separator & Color_Fg_Red));
   function SGR_Warning return String is
     (SGR_Seq (Color_Bold & Color_Separator & Color_Fg_Magenta));
   function SGR_Note return String is
     (SGR_Seq (Color_Bold & Color_Separator & Color_Fg_Cyan));
   function SGR_Locus return String is
     (SGR_Seq (Color_Bold));

   -----------------
   -- Subprograms --
   -----------------

   procedure Add_Class;
   --  Add 'Class to buffer for class wide type case (Class_Flag set)

   function Buffer_Ends_With (C : Character) return Boolean;
   --  Tests if message buffer ends with given character

   function Buffer_Ends_With (S : String) return Boolean;
   --  Tests if message buffer ends with given string preceded by a space

   procedure Buffer_Remove (C : Character);
   --  Remove given character from end of buffer if it is present

   procedure Buffer_Remove (S : String);
   --  Removes given string from end of buffer if it is present at end of
   --  buffer, and preceded by a space.

   function Compilation_Errors return Boolean;
   --  Returns true if errors have been detected, or warnings in -gnatwe
   --  (treat warnings as errors) mode.

   procedure dmsg (Id : Error_Msg_Id);
   --  Debugging routine to dump an error message

   procedure Debug_Output (N : Node_Id);
   --  Called from Error_Msg_N and Error_Msg_NE to generate line of debug
   --  output giving node number (of node N) if the debug X switch is set.

   procedure Check_Duplicate_Message (M1, M2 : Error_Msg_Id);
   --  This function is passed the Id values of two error messages. If either
   --  M1 or M2 is a continuation message, or is already deleted, the call is
   --  ignored. Otherwise a check is made to see if M1 and M2 are duplicated or
   --  redundant. If so, the message to be deleted and all its continuations
   --  are marked with the Deleted flag set to True.

   function Count_Compile_Time_Pragma_Warnings return Int;
   --  Returns the number of warnings in the Errors table that were triggered
   --  by a Compile_Time_Warning pragma.

   function Get_Warning_Option (Id : Error_Msg_Id) return String;
   --  Returns the warning switch causing this warning message or an empty
   --  string is there is none..

   function Get_Warning_Tag (Id : Error_Msg_Id) return String;
   --  Given an error message ID, return tag showing warning message class, or
   --  the null string if this option is not enabled or this is not a warning.

   function Matches (S : String; P : String) return Boolean;
   --  Returns true if the String S matches the pattern P, which can contain
   --  wildcard chars (*). The entire pattern must match the entire string.
   --  Case is ignored in the comparison (so X matches x).

   procedure Output_Error_Msgs (E : in out Error_Msg_Id);
   --  Output source line, error flag, and text of stored error message and all
   --  subsequent messages for the same line and unit. On return E is set to be
   --  one higher than the last message output.

   procedure Output_Line_Number (L : Logical_Line_Number);
   --  Output a line number as six digits (with leading zeroes suppressed),
   --  followed by a period and a blank (note that this is 8 characters which
   --  means that tabs in the source line will not get messed up). Line numbers
   --  that match or are less than the last Source_Reference pragma are listed
   --  as all blanks, avoiding output of junk line numbers.

   procedure Output_Msg_Text (E : Error_Msg_Id);
   --  Outputs characters of text in the text of the error message E. Note that
   --  no end of line is output, the caller is responsible for adding the end
   --  of line. If Error_Msg_Line_Length is non-zero, this is the routine that
   --  splits the line generating multiple lines of output, and in this case
   --  the last line has no terminating end of line character.

   procedure Prescan_Message (Msg : String);
   --  Scans message text and sets the following variables:
   --
   --    Is_Warning_Msg is set True if Msg is a warning message (contains a
   --    question mark character), and False otherwise.
   --
   --    Is_Style_Msg is set True if Msg is a style message (starts with
   --    "(style)") and False otherwise.
   --
   --    Is_Info_Msg is set True if Msg is an information message (starts
   --    with "info: ". Such messages must contain a ? sequence since they
   --    are also considered to be warning messages, and get a tag.
   --
   --    Is_Serious_Error is set to True unless the message is a warning or
   --    style message or contains the character | (non-serious error).
   --
   --    Is_Unconditional_Msg is set True if the message contains the character
   --    ! and is otherwise set False.
   --
   --    Has_Double_Exclam is set True if the message contains the sequence !!
   --    and is otherwise set False.
   --
   --    Has_Insertion_Line is set True if the message contains the character #
   --    and is otherwise set False.
   --
   --  We need to know right away these aspects of a message, since we will
   --  test these values before doing the full error scan.
   --
   --  Note that the call has no effect for continuation messages (those whose
   --  first character is '\'), and all variables are left unchanged, unless
   --  -gnatdF is set.

   procedure Purge_Messages (From : Source_Ptr; To : Source_Ptr);
   --  All error messages whose location is in the range From .. To (not
   --  including the end points) will be deleted from the error listing.

   function Same_Error (M1, M2 : Error_Msg_Id) return Boolean;
   --  See if two messages have the same text. Returns true if the text of the
   --  two messages is identical, or if one of them is the same as the other
   --  with an appended "instance at xxx" tag.

   procedure Set_Msg_Blank;
   --  Sets a single blank in the message if the preceding character is a
   --  non-blank character other than a left parenthesis or minus. Has no
   --  effect if manual quote mode is turned on.

   procedure Set_Msg_Blank_Conditional;
   --  Sets a single blank in the message if the preceding character is a
   --  non-blank character other than a left parenthesis or quote. Has no
   --  effect if manual quote mode is turned on.

   procedure Set_Msg_Char (C : Character);
   --  Add a single character to the current message. This routine does not
   --  check for special insertion characters (they are just treated as text
   --  characters if they occur).

   procedure Set_Msg_Insertion_File_Name;
   --  Handle file name insertion (left brace insertion character)

   procedure Set_Msg_Insertion_Line_Number (Loc, Flag : Source_Ptr);
   --  Handle line number insertion (# insertion character). Loc is the
   --  location to be referenced, and Flag is the location at which the
   --  flag is posted (used to determine whether to add "in file xxx")

   procedure Set_Msg_Insertion_Name_Literal;

   procedure Set_Msg_Insertion_Name;
   --  Handle name insertion (% insertion character)

   procedure Set_Msg_Insertion_Reserved_Name;
   --  Handle insertion of reserved word name (* insertion character)

   procedure Set_Msg_Insertion_Reserved_Word
     (Text : String;
      J    : in out Integer);
   --  Handle reserved word insertion (upper case letters). The Text argument
   --  is the current error message input text, and J is an index which on
   --  entry points to the first character of the reserved word, and on exit
   --  points past the last character of the reserved word. Note that RM and
   --  SPARK are treated specially and not considered to be keywords.

   procedure Set_Msg_Insertion_Run_Time_Name;
   --  If package System contains a definition for Run_Time_Name (see package
   --  Targparm for details), then this procedure will insert a message of
   --  the form (name) into the current error message, with name set in mixed
   --  case (upper case after any spaces). If no run time name is defined,
   --  then this routine has no effect).

   procedure Set_Msg_Insertion_Uint;
   --  Handle Uint insertion (^ insertion character)

   procedure Set_Msg_Int (Line : Int);
   --  Set the decimal representation of the argument in the error message
   --  buffer with no leading zeroes output.

   procedure Set_Msg_Name_Buffer;
   --  Output name from Namet.Global_Name_Buffer, with surrounding quotes
   --  unless manual quotation mode is in effect.

   procedure Set_Msg_Quote;
   --  Set quote if in normal quote mode, nothing if in manual quote mode

   procedure Set_Msg_Str (Text : String);
   --  Add a sequence of characters to the current message. This routine does
   --  not check for special insertion characters (they are just treated as
   --  text characters if they occur). It does perform the transformation of
   --  the special strings _xxx (xxx = Pre/Post/Type_Invariant) to xxx'Class.

   procedure Set_Next_Non_Deleted_Msg (E : in out Error_Msg_Id);
   --  Given a message id, move to next message id, but skip any deleted
   --  messages, so that this results in E on output being the first non-
   --  deleted message following the input value of E, or No_Error_Msg if
   --  the input value of E was either already No_Error_Msg, or was the
   --  last non-deleted message.

   procedure Set_Specific_Warning_Off
     (Loc    : Source_Ptr;
      Msg    : String;
      Reason : String_Id;
      Config : Boolean;
      Used   : Boolean := False);
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
      Err : out Boolean);
   --  This is called in response to the two argument form of pragma Warnings
   --  where the first argument is ON, and the second argument is a string
   --  which identifies a specific warning to be suppressed. The first argument
   --  is the end of the suppression range, and the second argument is the
   --  string from the pragma. Err is set to True on return to report the error
   --  of no matching Warnings Off pragma preceding this one.

   procedure Set_Warnings_Mode_Off (Loc : Source_Ptr; Reason : String_Id);
   --  Called in response to a pragma Warnings (Off) to record the source
   --  location from which warnings are to be turned off. Reason is the
   --  Reason from the pragma, or the null string if none is given.

   procedure Set_Warnings_Mode_On (Loc : Source_Ptr);
   --  Called in response to a pragma Warnings (On) to record the source
   --  location from which warnings are to be turned back on.

   function Warnings_Suppressed (Loc : Source_Ptr) return String_Id;
   --  Determines if given location is covered by a warnings off suppression
   --  range in the warnings table (or is suppressed by compilation option,
   --  which generates a warning range for the whole source file). This routine
   --  only deals with the general ON/OFF case, not specific warnings. The
   --  returned result is No_String if warnings are not suppressed. If warnings
   --  are suppressed for the given location, then corresponding Reason
   --  parameter from the pragma is returned (or the null string if no Reason
   --  parameter was present).

   function Warning_Specifically_Suppressed
     (Loc : Source_Ptr;
      Msg : String_Ptr;
      Tag : String := "") return String_Id;
   --  Determines if given message to be posted at given location is suppressed
   --  by specific ON/OFF Warnings pragmas specifying this particular message.
   --  If the warning is not suppressed then No_String is returned, otherwise
   --  the corresponding warning string is returned (or the null string if no
   --  Warning argument was present in the pragma). Tag is the error message
   --  tag for the message in question or the null string if there is no tag.
   --
   --  Note: we have a null default for Tag to deal with calls from an old
   --  branch of gnat2why, which does not know about tags in the calls but
   --  which uses the latest version of erroutc.

   function Warning_Treated_As_Error (Msg : String) return Boolean;
   --  Returns True if the warning message Msg matches any of the strings
   --  given by Warning_As_Error pragmas, as stored in the Warnings_As_Errors
   --  table.

   type Error_Msg_Proc is
     access procedure (Msg : String; Flag_Location : Source_Ptr);
   procedure Validate_Specific_Warnings (Eproc : Error_Msg_Proc);
   --  Checks that specific warnings are consistent (for non-configuration
   --  case, properly closed, and used). The argument is a pointer to the
   --  Error_Msg procedure to be called if any inconsistencies are detected.

end Erroutc;

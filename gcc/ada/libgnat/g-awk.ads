------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              G N A T . A W K                             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2000-2024, AdaCore                     --
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

--  This is an AWK-like unit. It provides an easy interface for parsing one
--  or more files containing formatted data. The file can be viewed seen as
--  a database where each record is a line and a field is a data element in
--  this line. In this implementation an AWK record is a line. This means
--  that a record cannot span multiple lines. The operating procedure is to
--  read files line by line, with each line being presented to the user of
--  the package. The interface provides services to access specific fields
--  in the line. Thus it is possible to control actions taken on a line based
--  on values of some fields. This can be achieved directly or by registering
--  callbacks triggered on programmed conditions.
--
--  The state of an AWK run is recorded in an object of type session.
--  The following is the procedure for using a session to control an
--  AWK run:
--
--     1) Specify which session is to be used. It is possible to use the
--        default session or to create a new one by declaring an object of
--        type Session_Type. For example:
--
--           Computers : Session_Type;
--
--     2) Specify how to cut a line into fields. There are two modes: using
--        character fields separators or column width. This is done by using
--        Set_Fields_Separators or Set_Fields_Width. For example by:
--
--           AWK.Set_Field_Separators (";,", Computers);
--
--        or by using iterators' Separators parameter.
--
--     3) Specify which files to parse. This is done with Add_File/Add_Files
--        services, or by using the iterators' Filename parameter. For
--        example:
--
--           AWK.Add_File ("myfile.db", Computers);
--
--     4) Run the AWK session using one of the provided iterators.
--
--           Parse
--              This is the most automated iterator. You can gain control on
--              the session only by registering one or more callbacks (see
--              Register).
--
--           Get_Line/End_Of_Data
--              This is a manual iterator to be used with a loop. You have
--              complete control on the session. You can use callbacks but
--              this is not required.
--
--           For_Every_Line
--              This provides a mixture of manual/automated iterator action.
--
--        Examples of these three approaches appear below
--
--  There are many ways to use this package. The following discussion shows
--  three approaches to using this package, using the three iterator forms.
--  All examples will use the following file (computer.db):
--
--     Pluton;Windows-NT;Pentium III
--     Mars;Linux;Pentium Pro
--     Venus;Solaris;Sparc
--     Saturn;OS/2;i486
--     Jupiter;MacOS;PPC
--
--  1) Using Parse iterator
--
--     Here the first step is to register some action associated to a pattern
--     and then to call the Parse iterator (this is the simplest way to use
--     this unit). The default session is used here. For example to output the
--     second field (the OS) of computer "Saturn".
--
--           procedure Action is
--           begin
--              Put_Line (AWK.Field (2));
--           end Action;
--
--        begin
--           AWK.Register (1, "Saturn", Action'Access);
--           AWK.Parse (";", "computer.db");
--
--
--  2) Using the Get_Line/End_Of_Data iterator
--
--     Here you have full control. For example to do the same as
--     above but using a specific session, you could write:
--
--           Computer_File : Session_Type;
--
--        begin
--           AWK.Set_Current (Computer_File);
--           AWK.Open (Separators => ";",
--                     Filename   => "computer.db");
--
--           --  Display Saturn OS
--
--           while not AWK.End_Of_File loop
--              AWK.Get_Line;
--
--              if AWK.Field (1) = "Saturn" then
--                 Put_Line (AWK.Field (2));
--              end if;
--           end loop;
--
--           AWK.Close (Computer_File);
--
--
--  3) Using For_Every_Line iterator
--
--     In this case you use a provided iterator and you pass the procedure
--     that must be called for each record. You could code the previous
--     example could be coded as follows (using the iterator quick interface
--     but without using the current session):
--
--           Computer_File : Session_Type;
--
--           procedure Action (Quit : in out Boolean) is
--           begin
--              if AWK.Field (1, Computer_File) = "Saturn" then
--                 Put_Line (AWK.Field (2, Computer_File));
--              end if;
--           end Action;
--
--           procedure Look_For_Saturn is
--              new AWK.For_Every_Line (Action);
--
--        begin
--           Look_For_Saturn (Separators => ";",
--                            Filename   => "computer.db",
--                            Session    => Computer_File);
--
--           Integer_Text_IO.Put
--             (Integer (AWK.NR (Session => Computer_File)));
--           Put_Line (" line(s) have been processed.");
--
--  You can also use a regular expression for the pattern. Let us output
--  the computer name for all computer for which the OS has a character
--  O in its name.
--
--           Regexp   : String := ".*O.*";
--
--           Matcher  : Regpat.Pattern_Matcher := Regpat.Compile (Regexp);
--
--           procedure Action is
--           begin
--              Text_IO.Put_Line (AWK.Field (2));
--           end Action;
--
--        begin
--           AWK.Register (2, Matcher, Action'Unrestricted_Access);
--           AWK.Parse (";", "computer.db");
--

with Ada.Finalization;
with GNAT.Regpat;

package GNAT.AWK is

   Session_Error : exception;
   --  Raised when a Session is reused but is not closed

   File_Error : exception;
   --  Raised when there is a file problem (see below)

   End_Error : exception;
   --  Raised when an attempt is made to read beyond the end of the last
   --  file of a session.

   Field_Error : exception;
   --  Raised when accessing a field value which does not exist

   Data_Error : exception;
   --  Raised when it is impossible to convert a field value to a specific type

   type Count is new Natural;

   type Widths_Set is array (Positive range <>) of Positive;
   --  Used to store a set of columns widths

   Default_Separators : constant String := " " & ASCII.HT;

   Use_Current : constant String := "";
   --  Value used when no separator or filename is specified in iterators

   type Session_Type is limited private;
   --  This is the main exported type. A session is used to keep the state of
   --  a full AWK run. The state comprises a list of files, the current file,
   --  the number of line processed, the current line, the number of fields in
   --  the current line... A default session is provided (see Set_Current,
   --  Current_Session and Default_Session below).

   ----------------------------
   -- Package initialization --
   ----------------------------

   --  To be thread safe it is not possible to use the default provided
   --  session. Each task must used a specific session and specify it
   --  explicitly for every services.

   procedure Set_Current (Session : Session_Type);
   --  Set the session to be used by default. This file will be used when the
   --  Session parameter in following services is not specified.

   function Current_Session return not null access Session_Type;
   --  Returns the session used by default by all services. This is the
   --  latest session specified by Set_Current service or the session
   --  provided by default with this implementation.

   function Default_Session return not null access Session_Type;
   --  Returns the default session provided by this package. Note that this is
   --  the session return by Current_Session if Set_Current has not been used.

   procedure Set_Field_Separators
     (Separators : String       := Default_Separators;
      Session    : Session_Type);
   procedure Set_Field_Separators
     (Separators : String       := Default_Separators);
   --  Set the field separators. Each character in the string is a field
   --  separator. When a line is read it will be split by field using the
   --  separators set here. Separators can be changed at any point and in this
   --  case the current line is split according to the new separators. In the
   --  special case that Separators is a space and a tabulation
   --  (Default_Separators), fields are separated by runs of spaces and/or
   --  tabs.

   procedure Set_FS
     (Separators : String       := Default_Separators;
      Session    : Session_Type)
     renames Set_Field_Separators;
   procedure Set_FS
     (Separators : String       := Default_Separators)
     renames Set_Field_Separators;
   --  FS is the AWK abbreviation for above service

   procedure Set_Field_Widths
     (Field_Widths : Widths_Set;
      Session      : Session_Type);
   procedure Set_Field_Widths
     (Field_Widths : Widths_Set);
   --  This is another way to split a line by giving the length (in number of
   --  characters) of each field in a line. Field widths can be changed at any
   --  point and in this case the current line is split according to the new
   --  field lengths. A line split with this method must have a length equal or
   --  greater to the total of the field widths. All characters remaining on
   --  the line after the latest field are added to a new automatically
   --  created field.

   procedure Add_File
     (Filename : String;
      Session  : Session_Type);
   procedure Add_File
     (Filename : String);
   --  Add Filename to the list of file to be processed. There is no limit on
   --  the number of files that can be added. Files are processed in the order
   --  they have been added (i.e. the filename list is FIFO). If Filename does
   --  not exist or if it is not readable, File_Error is raised.

   procedure Add_Files
     (Directory             : String;
      Filenames             : String;
      Number_Of_Files_Added : out Natural;
      Session               : Session_Type);
   procedure Add_Files
     (Directory             : String;
      Filenames             : String;
      Number_Of_Files_Added : out Natural);
   --  Add all files matching the regular expression Filenames in the specified
   --  directory to the list of file to be processed. There is no limit on
   --  the number of files that can be added. Each file is processed in
   --  the same order they have been added (i.e. the filename list is FIFO).
   --  The number of files (possibly 0) added is returned in
   --  Number_Of_Files_Added.

   -------------------------------------
   -- Information about current state --
   -------------------------------------

   function Number_Of_Fields
     (Session : Session_Type) return Count;
   function Number_Of_Fields
     return Count;
   pragma Inline (Number_Of_Fields);
   --  Returns the number of fields in the current record. It returns 0 when
   --  no file is being processed.

   function NF
     (Session : Session_Type) return Count
     renames Number_Of_Fields;
   function NF
     return Count
     renames Number_Of_Fields;
   --  AWK abbreviation for above service

   function Number_Of_File_Lines
     (Session : Session_Type) return Count;
   function Number_Of_File_Lines
     return Count;
   pragma Inline (Number_Of_File_Lines);
   --  Returns the current line number in the processed file. It returns 0 when
   --  no file is being processed.

   function FNR (Session : Session_Type) return Count
     renames Number_Of_File_Lines;
   function FNR return Count
     renames Number_Of_File_Lines;
   --  AWK abbreviation for above service

   function Number_Of_Lines
     (Session : Session_Type) return Count;
   function Number_Of_Lines
     return Count;
   pragma Inline (Number_Of_Lines);
   --  Returns the number of line processed until now. This is equal to number
   --  of line in each already processed file plus FNR. It returns 0 when
   --  no file is being processed.

   function NR (Session : Session_Type) return Count
     renames Number_Of_Lines;
   function NR return Count
     renames Number_Of_Lines;
   --  AWK abbreviation for above service

   function Number_Of_Files
     (Session : Session_Type) return Natural;
   function Number_Of_Files
     return Natural;
   pragma Inline (Number_Of_Files);
   --  Returns the number of files associated with Session. This is the total
   --  number of files added with Add_File and Add_Files services.

   function File (Session : Session_Type) return String;
   function File return String;
   --  Returns the name of the file being processed. It returns the empty
   --  string when no file is being processed.

   ---------------------
   -- Field accessors --
   ---------------------

   function Field
     (Rank    : Count;
      Session : Session_Type) return String;
   function Field
     (Rank    : Count) return String;
   --  Returns field number Rank value of the current record. If Rank = 0 it
   --  returns the current record (i.e. the line as read in the file). It
   --  raises Field_Error if Rank > NF or if Session is not open.

   function Field
     (Rank    : Count;
      Session : Session_Type) return Integer;
   function Field
     (Rank    : Count) return Integer;
   --  Returns field number Rank value of the current record as an integer. It
   --  raises Field_Error if Rank > NF or if Session is not open. It
   --  raises Data_Error if the field value cannot be converted to an integer.

   function Field
     (Rank    : Count;
      Session : Session_Type) return Float;
   function Field
     (Rank    : Count) return Float;
   --  Returns field number Rank value of the current record as a float. It
   --  raises Field_Error if Rank > NF or if Session is not open. It
   --  raises Data_Error if the field value cannot be converted to a float.

   generic
      type Discrete is (<>);
   function Discrete_Field
     (Rank    : Count;
      Session : Session_Type) return Discrete;
   generic
      type Discrete is (<>);
   function Discrete_Field_Current_Session
     (Rank    : Count) return Discrete;
   --  Returns field number Rank value of the current record as a type
   --  Discrete. It raises Field_Error if Rank > NF. It raises Data_Error if
   --  the field value cannot be converted to type Discrete.

   --------------------
   -- Pattern/Action --
   --------------------

   --  AWK defines rules like "PATTERN { ACTION }". Which means that ACTION
   --  will be executed if PATTERN match. A pattern in this implementation can
   --  be a simple string (match function is equality), a regular expression,
   --  a function returning a boolean. An action is associated to a pattern
   --  using the Register services.
   --
   --  Each procedure Register will add a rule to the set of rules for the
   --  session. Rules are examined in the order they have been added.

   type Pattern_Callback is access function return Boolean;
   --  This is a pattern function pointer. When it returns True the associated
   --  action will be called.

   type Action_Callback is access procedure;
   --  A simple action pointer

   type Match_Action_Callback is
     access procedure (Matches : GNAT.Regpat.Match_Array);
   --  An advanced action pointer used with a regular expression pattern. It
   --  returns an array of all the matches. See GNAT.Regpat for further
   --  information.

   procedure Register
     (Field   : Count;
      Pattern : String;
      Action  : Action_Callback;
      Session : Session_Type);
   procedure Register
     (Field   : Count;
      Pattern : String;
      Action  : Action_Callback);
   --  Register an Action associated with a Pattern. The pattern here is a
   --  simple string that must match exactly the field number specified.

   procedure Register
     (Field   : Count;
      Pattern : GNAT.Regpat.Pattern_Matcher;
      Action  : Action_Callback;
      Session : Session_Type);
   procedure Register
     (Field   : Count;
      Pattern : GNAT.Regpat.Pattern_Matcher;
      Action  : Action_Callback);
   --  Register an Action associated with a Pattern. The pattern here is a
   --  simple regular expression which must match the field number specified.

   procedure Register
     (Field   : Count;
      Pattern : GNAT.Regpat.Pattern_Matcher;
      Action  : Match_Action_Callback;
      Session : Session_Type);
   procedure Register
     (Field   : Count;
      Pattern : GNAT.Regpat.Pattern_Matcher;
      Action  : Match_Action_Callback);
   --  Same as above but it pass the set of matches to the action
   --  procedure. This is useful to analyze further why and where a regular
   --  expression did match.

   procedure Register
     (Pattern : Pattern_Callback;
      Action  : Action_Callback;
      Session : Session_Type);
   procedure Register
     (Pattern : Pattern_Callback;
      Action  : Action_Callback);
   --  Register an Action associated with a Pattern. The pattern here is a
   --  function that must return a boolean. Action callback will be called if
   --  the pattern callback returns True and nothing will happen if it is
   --  False. This version is more general, the two other register services
   --  trigger an action based on the value of a single field only.

   procedure Register
     (Action  : Action_Callback;
      Session : Session_Type);
   procedure Register
     (Action  : Action_Callback);
   --  Register an Action that will be called for every line. This is
   --  equivalent to a Pattern_Callback function always returning True.

   --------------------
   -- Parse iterator --
   --------------------

   procedure Parse
     (Separators : String := Use_Current;
      Filename   : String := Use_Current;
      Session    : Session_Type);
   procedure Parse
     (Separators : String := Use_Current;
      Filename   : String := Use_Current);
   --  Launch the iterator, it will read every line in all specified
   --  session's files. Registered callbacks are then called if the associated
   --  pattern match. It is possible to specify a filename and a set of
   --  separators directly. This offer a quick way to parse a single
   --  file. These parameters will override those specified by Set_FS and
   --  Add_File. The Session will be opened and closed automatically.
   --  File_Error is raised if there is no file associated with Session, or if
   --  a file associated with Session is not longer readable. It raises
   --  Session_Error is Session is already open.

   -----------------------------------
   -- Get_Line/End_Of_Data Iterator --
   -----------------------------------

   type Callback_Mode is (None, Only, Pass_Through);
   --  These mode are used for Get_Line/End_Of_Data and For_Every_Line
   --  iterators. The associated semantic is:
   --
   --    None
   --       callbacks are not active. This is the default mode for
   --       Get_Line/End_Of_Data and For_Every_Line iterators.
   --
   --    Only
   --       callbacks are active, if at least one pattern match, the associated
   --       action is called and this line will not be passed to the user. In
   --       the Get_Line case the next line will be read (if there is some
   --       line remaining), in the For_Every_Line case Action will
   --       not be called for this line.
   --
   --    Pass_Through
   --       callbacks are active, for patterns which match the associated
   --       action is called. Then the line is passed to the user. It means
   --       that Action procedure is called in the For_Every_Line case and
   --       that Get_Line returns with the current line active.
   --

   procedure Open
     (Separators : String := Use_Current;
      Filename   : String := Use_Current;
      Session    : Session_Type);
   procedure Open
     (Separators : String := Use_Current;
      Filename   : String := Use_Current);
   --  Open the first file and initialize the unit. This must be called once
   --  before using Get_Line. It is possible to specify a filename and a set of
   --  separators directly. This offer a quick way to parse a single file.
   --  These parameters will override those specified by Set_FS and Add_File.
   --  File_Error is raised if there is no file associated with Session, or if
   --  the first file associated with Session is no longer readable. It raises
   --  Session_Error is Session is already open.

   procedure Get_Line
     (Callbacks : Callback_Mode := None;
      Session   : Session_Type);
   procedure Get_Line
     (Callbacks : Callback_Mode := None);
   --  Read a line from the current input file. If the file index is at the
   --  end of the current input file (i.e. End_Of_File is True) then the
   --  following file is opened. If there is no more file to be processed,
   --  exception End_Error will be raised. File_Error will be raised if Open
   --  has not been called. Next call to Get_Line will return the following
   --  line in the file. By default the registered callbacks are not called by
   --  Get_Line, this can activated by setting Callbacks (see Callback_Mode
   --  description above). File_Error may be raised if a file associated with
   --  Session is not readable.
   --
   --  When Callbacks is not None, it is possible to exhaust all the lines
   --  of all the files associated with Session. In this case, File_Error
   --  is not raised.
   --
   --  This procedure can be used from a subprogram called by procedure Parse
   --  or by an instantiation of For_Every_Line (see below).

   function End_Of_Data
     (Session : Session_Type) return Boolean;
   function End_Of_Data
     return Boolean;
   pragma Inline (End_Of_Data);
   --  Returns True if there is no more data to be processed in Session. It
   --  means that the latest session's file is being processed and that
   --  there is no more data to be read in this file (End_Of_File is True).

   function End_Of_File
     (Session : Session_Type) return Boolean;
   function End_Of_File
     return Boolean;
   pragma Inline (End_Of_File);
   --  Returns True when there is no more data to be processed on the current
   --  session's file.

   procedure Close (Session : Session_Type);
   --  Release all associated data with Session. All memory allocated will
   --  be freed, the current file will be closed if needed, the callbacks
   --  will be unregistered. Close is convenient in reestablishing a session
   --  for new use. Get_Line is no longer usable (will raise File_Error)
   --  except after a successful call to Open, Parse or an instantiation
   --  of For_Every_Line.

   -----------------------------
   -- For_Every_Line iterator --
   -----------------------------

   generic
      with procedure Action (Quit : in out Boolean);
   procedure For_Every_Line
     (Separators : String := Use_Current;
      Filename   : String := Use_Current;
      Callbacks  : Callback_Mode := None;
      Session    : Session_Type);
   generic
      with procedure Action (Quit : in out Boolean);
   procedure For_Every_Line_Current_Session
     (Separators : String := Use_Current;
      Filename   : String := Use_Current;
      Callbacks  : Callback_Mode := None);
   --  This is another iterator. Action will be called for each new
   --  record. The iterator's termination can be controlled by setting Quit
   --  to True. It is by default set to False. It is possible to specify a
   --  filename and a set of separators directly. This offer a quick way to
   --  parse a single file. These parameters will override those specified by
   --  Set_FS and Add_File. By default the registered callbacks are not called
   --  by For_Every_Line, this can activated by setting Callbacks (see
   --  Callback_Mode description above). The Session will be opened and
   --  closed automatically. File_Error is raised if there is no file
   --  associated with Session. It raises Session_Error is Session is already
   --  open.

private
   type Session_Data;
   type Session_Data_Access is access Session_Data;

   type Session_Type is new Ada.Finalization.Limited_Controlled with record
      Data : Session_Data_Access;
      Self : not null access Session_Type := Session_Type'Unchecked_Access;
   end record;

   procedure Initialize (Session : in out Session_Type);
   procedure Finalize   (Session : in out Session_Type);

end GNAT.AWK;

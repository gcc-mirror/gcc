------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              G N A T . A W K                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2000-2019, AdaCore                     --
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

with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Unchecked_Deallocation;

with GNAT.Directory_Operations;
with GNAT.Dynamic_Tables;
with GNAT.OS_Lib;

package body GNAT.AWK is

   use Ada;
   use Ada.Strings.Unbounded;

   -----------------------
   -- Local subprograms --
   -----------------------

   --  The following two subprograms provide a functional interface to the
   --  two special session variables, that are manipulated explicitly by
   --  Finalize, but must be declared after Finalize to prevent static
   --  elaboration warnings.

   function Get_Def return Session_Data_Access;
   procedure Set_Cur;

   ----------------
   -- Split mode --
   ----------------

   package Split is

      type Mode is abstract tagged null record;
      --  This is the main type which is declared abstract. This type must be
      --  derived for each split style.

      type Mode_Access is access Mode'Class;

      procedure Current_Line (S : Mode; Session : Session_Type)
        is abstract;
      --  Split current line of Session using split mode S

      ------------------------
      -- Split on separator --
      ------------------------

      type Separator (Size : Positive) is new Mode with record
         Separators : String (1 .. Size);
      end record;

      procedure Current_Line
        (S       : Separator;
         Session : Session_Type);

      ---------------------
      -- Split on column --
      ---------------------

      type Column (Size : Positive) is new Mode with record
         Columns : Widths_Set (1 .. Size);
      end record;

      procedure Current_Line (S : Column; Session : Session_Type);

   end Split;

   procedure Free is new Unchecked_Deallocation
     (Split.Mode'Class, Split.Mode_Access);

   ----------------
   -- File_Table --
   ----------------

   type AWK_File is access String;

   package File_Table is
      new Dynamic_Tables (AWK_File, Natural, 1, 5, 50);
   --  List of file names associated with a Session

   procedure Free is new Unchecked_Deallocation (String, AWK_File);

   -----------------
   -- Field_Table --
   -----------------

   type Field_Slice is record
      First : Positive;
      Last  : Natural;
   end record;
   --  This is a field slice (First .. Last) in session's current line

   package Field_Table is
      new Dynamic_Tables (Field_Slice, Natural, 1, 10, 100);
   --  List of fields for the current line

   --------------
   -- Patterns --
   --------------

   --  Define all patterns style: exact string, regular expression, boolean
   --  function.

   package Patterns is

      type Pattern is abstract tagged null record;
      --  This is the main type which is declared abstract. This type must be
      --  derived for each patterns style.

      type Pattern_Access is access Pattern'Class;

      function Match
        (P       : Pattern;
         Session : Session_Type) return Boolean
      is abstract;
      --  Returns True if P match for the current session and False otherwise

      procedure Release (P : in out Pattern);
      --  Release memory used by the pattern structure

      --------------------------
      -- Exact string pattern --
      --------------------------

      type String_Pattern is new Pattern with record
         Str  : Unbounded_String;
         Rank : Count;
      end record;

      function Match
        (P       : String_Pattern;
         Session : Session_Type) return Boolean;

      --------------------------------
      -- Regular expression pattern --
      --------------------------------

      type Pattern_Matcher_Access is access Regpat.Pattern_Matcher;

      type Regexp_Pattern is new Pattern with record
         Regx : Pattern_Matcher_Access;
         Rank : Count;
      end record;

      function Match
        (P       : Regexp_Pattern;
         Session : Session_Type) return Boolean;

      procedure Release (P : in out Regexp_Pattern);

      ------------------------------
      -- Boolean function pattern --
      ------------------------------

      type Callback_Pattern is new Pattern with record
         Pattern : Pattern_Callback;
      end record;

      function Match
        (P       : Callback_Pattern;
         Session : Session_Type) return Boolean;

   end Patterns;

   procedure Free is new Unchecked_Deallocation
     (Patterns.Pattern'Class, Patterns.Pattern_Access);

   -------------
   -- Actions --
   -------------

   --  Define all action style : simple call, call with matches

   package Actions is

      type Action is abstract tagged null record;
      --  This is the main type which is declared abstract. This type must be
      --  derived for each action style.

      type Action_Access is access Action'Class;

      procedure Call
        (A       : Action;
         Session : Session_Type) is abstract;
      --  Call action A as required

      -------------------
      -- Simple action --
      -------------------

      type Simple_Action is new Action with record
         Proc : Action_Callback;
      end record;

      procedure Call
        (A       : Simple_Action;
         Session : Session_Type);

      -------------------------
      -- Action with matches --
      -------------------------

      type Match_Action is new Action with record
         Proc : Match_Action_Callback;
      end record;

      procedure Call
        (A       : Match_Action;
         Session : Session_Type);

   end Actions;

   procedure Free is new Unchecked_Deallocation
     (Actions.Action'Class, Actions.Action_Access);

   --------------------------
   -- Pattern/Action table --
   --------------------------

   type Pattern_Action is record
      Pattern : Patterns.Pattern_Access;  -- If Pattern is True
      Action  : Actions.Action_Access;    -- Action will be called
   end record;

   package Pattern_Action_Table is
      new Dynamic_Tables (Pattern_Action, Natural, 1, 5, 50);

   ------------------
   -- Session Data --
   ------------------

   type Session_Data is record
      Current_File : Text_IO.File_Type;
      Current_Line : Unbounded_String;
      Separators   : Split.Mode_Access;
      Files        : File_Table.Instance;
      File_Index   : Natural := 0;
      Fields       : Field_Table.Instance;
      Filters      : Pattern_Action_Table.Instance;
      NR           : Natural := 0;
      FNR          : Natural := 0;
      Matches      : Regpat.Match_Array (0 .. 100);
      --  Latest matches for the regexp pattern
   end record;

   procedure Free is
      new Unchecked_Deallocation (Session_Data, Session_Data_Access);

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Session : in out Session_Type) is
   begin
      --  We release the session data only if it is not the default session

      if Session.Data /= Get_Def then
         --  Release separators

         Free (Session.Data.Separators);

         Free (Session.Data);

         --  Since we have closed the current session, set it to point now to
         --  the default session.

         Set_Cur;
      end if;
   end Finalize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Session : in out Session_Type) is
   begin
      Session.Data := new Session_Data;

      --  Initialize separators

      Session.Data.Separators :=
        new Split.Separator'(Default_Separators'Length, Default_Separators);

      --  Initialize all tables

      File_Table.Init  (Session.Data.Files);
      Field_Table.Init (Session.Data.Fields);
      Pattern_Action_Table.Init (Session.Data.Filters);
   end Initialize;

   -----------------------
   -- Session Variables --
   -----------------------

   Def_Session : Session_Type;
   Cur_Session : Session_Type;

   ----------------------
   -- Private Services --
   ----------------------

   function Always_True return Boolean;
   --  A function that always returns True

   function Apply_Filters
     (Session : Session_Type) return Boolean;
   --  Apply any filters for which the Pattern is True for Session. It returns
   --  True if a least one filters has been applied (i.e. associated action
   --  callback has been called).

   procedure Open_Next_File
     (Session : Session_Type);
   pragma Inline (Open_Next_File);
   --  Open next file for Session closing current file if needed. It raises
   --  End_Error if there is no more file in the table.

   procedure Raise_With_Info
     (E       : Exceptions.Exception_Id;
      Message : String;
      Session : Session_Type);
   pragma No_Return (Raise_With_Info);
   --  Raises exception E with the message prepended with the current line
   --  number and the filename if possible.

   procedure Read_Line (Session : Session_Type);
   --  Read a line for the Session and set Current_Line

   procedure Split_Line (Session : Session_Type);
   --  Split session's Current_Line according to the session separators and
   --  set the Fields table. This procedure can be called at any time.

   ----------------------
   -- Private Packages --
   ----------------------

   -------------
   -- Actions --
   -------------

   package body Actions is

      ----------
      -- Call --
      ----------

      procedure Call
        (A       : Simple_Action;
         Session : Session_Type)
      is
         pragma Unreferenced (Session);
      begin
         A.Proc.all;
      end Call;

      ----------
      -- Call --
      ----------

      procedure Call
        (A       : Match_Action;
         Session : Session_Type)
      is
      begin
         A.Proc (Session.Data.Matches);
      end Call;

   end Actions;

   --------------
   -- Patterns --
   --------------

   package body Patterns is

      -----------
      -- Match --
      -----------

      function Match
        (P       : String_Pattern;
         Session : Session_Type) return Boolean
      is
      begin
         return P.Str = Field (P.Rank, Session);
      end Match;

      -----------
      -- Match --
      -----------

      function Match
        (P       : Regexp_Pattern;
         Session : Session_Type) return Boolean
      is
         use type Regpat.Match_Location;
      begin
         Regpat.Match
           (P.Regx.all, Field (P.Rank, Session), Session.Data.Matches);
         return Session.Data.Matches (0) /= Regpat.No_Match;
      end Match;

      -----------
      -- Match --
      -----------

      function Match
        (P       : Callback_Pattern;
         Session : Session_Type) return Boolean
      is
         pragma Unreferenced (Session);
      begin
         return P.Pattern.all;
      end Match;

      -------------
      -- Release --
      -------------

      procedure Release (P : in out Pattern) is
         pragma Unreferenced (P);
      begin
         null;
      end Release;

      -------------
      -- Release --
      -------------

      procedure Release (P : in out Regexp_Pattern) is
         procedure Free is new Unchecked_Deallocation
           (Regpat.Pattern_Matcher, Pattern_Matcher_Access);
      begin
         Free (P.Regx);
      end Release;

   end Patterns;

   -----------
   -- Split --
   -----------

   package body Split is

      use Ada.Strings;

      ------------------
      -- Current_Line --
      ------------------

      procedure Current_Line (S : Separator; Session : Session_Type) is
         Line   : constant String := To_String (Session.Data.Current_Line);
         Fields : Field_Table.Instance renames Session.Data.Fields;
         Seps   : constant Maps.Character_Set := Maps.To_Set (S.Separators);

         Start  : Natural;
         Stop   : Natural;

      begin
         --  First field start here

         Start := Line'First;

         --  Record the first field start position which is the first character
         --  in the line.

         Field_Table.Increment_Last (Fields);
         Fields.Table (Field_Table.Last (Fields)).First := Start;

         loop
            --  Look for next separator

            Stop := Fixed.Index
              (Source => Line (Start .. Line'Last),
               Set    => Seps);

            exit when Stop = 0;

            Fields.Table (Field_Table.Last (Fields)).Last := Stop - 1;

            --  If separators are set to the default (space and tab) we skip
            --  all spaces and tabs following current field.

            if S.Separators = Default_Separators then
               Start := Fixed.Index
                 (Line (Stop + 1 .. Line'Last),
                  Maps.To_Set (Default_Separators),
                  Outside,
                  Strings.Forward);

               if Start = 0 then
                  Start := Stop + 1;
               end if;

            else
               Start := Stop + 1;
            end if;

            --  Record in the field table the start of this new field

            Field_Table.Increment_Last (Fields);
            Fields.Table (Field_Table.Last (Fields)).First := Start;

         end loop;

         Fields.Table (Field_Table.Last (Fields)).Last := Line'Last;
      end Current_Line;

      ------------------
      -- Current_Line --
      ------------------

      procedure Current_Line (S : Column; Session : Session_Type) is
         Line   : constant String := To_String (Session.Data.Current_Line);
         Fields : Field_Table.Instance renames Session.Data.Fields;
         Start  : Positive := Line'First;

      begin
         --  Record the first field start position which is the first character
         --  in the line.

         for C in 1 .. S.Columns'Length loop

            Field_Table.Increment_Last (Fields);

            Fields.Table (Field_Table.Last (Fields)).First := Start;

            Start := Start + S.Columns (C);

            Fields.Table (Field_Table.Last (Fields)).Last := Start - 1;

         end loop;

         --  If there is some remaining character on the line, add them in a
         --  new field.

         if Start - 1 < Line'Length then

            Field_Table.Increment_Last (Fields);

            Fields.Table (Field_Table.Last (Fields)).First := Start;

            Fields.Table (Field_Table.Last (Fields)).Last := Line'Last;
         end if;
      end Current_Line;

   end Split;

   --------------
   -- Add_File --
   --------------

   procedure Add_File
     (Filename : String;
      Session  : Session_Type)
   is
      Files : File_Table.Instance renames Session.Data.Files;

   begin
      if OS_Lib.Is_Regular_File (Filename) then
         File_Table.Increment_Last (Files);
         Files.Table (File_Table.Last (Files)) := new String'(Filename);
      else
         Raise_With_Info
           (File_Error'Identity,
            "File " & Filename & " not found.",
            Session);
      end if;
   end Add_File;

   procedure Add_File
     (Filename : String)
   is

   begin
      Add_File (Filename, Cur_Session);
   end Add_File;

   ---------------
   -- Add_Files --
   ---------------

   procedure Add_Files
     (Directory             : String;
      Filenames             : String;
      Number_Of_Files_Added : out Natural;
      Session               : Session_Type)
   is
      use Directory_Operations;

      Dir      : Dir_Type;
      Filename : String (1 .. 200);
      Last     : Natural;

   begin
      Number_Of_Files_Added := 0;

      Open (Dir, Directory);

      loop
         Read (Dir, Filename, Last);
         exit when Last = 0;

         Add_File (Filename (1 .. Last), Session);
         Number_Of_Files_Added := Number_Of_Files_Added + 1;
      end loop;

      Close (Dir);

   exception
      when others =>
         Raise_With_Info
           (File_Error'Identity,
            "Error scanning directory " & Directory
            & " for files " & Filenames & '.',
            Session);
   end Add_Files;

   procedure Add_Files
     (Directory             : String;
      Filenames             : String;
      Number_Of_Files_Added : out Natural)
   is

   begin
      Add_Files (Directory, Filenames, Number_Of_Files_Added, Cur_Session);
   end Add_Files;

   -----------------
   -- Always_True --
   -----------------

   function Always_True return Boolean is
   begin
      return True;
   end Always_True;

   -------------------
   -- Apply_Filters --
   -------------------

   function Apply_Filters
     (Session : Session_Type) return Boolean
   is
      Filters : Pattern_Action_Table.Instance renames Session.Data.Filters;
      Results : Boolean := False;

   begin
      --  Iterate through the filters table, if pattern match call action

      for F in 1 .. Pattern_Action_Table.Last (Filters) loop
         if Patterns.Match (Filters.Table (F).Pattern.all, Session) then
            Results := True;
            Actions.Call (Filters.Table (F).Action.all, Session);
         end if;
      end loop;

      return Results;
   end Apply_Filters;

   -----------
   -- Close --
   -----------

   procedure Close (Session : Session_Type) is
      Filters : Pattern_Action_Table.Instance renames Session.Data.Filters;
      Files   : File_Table.Instance renames Session.Data.Files;

   begin
      --  Close current file if needed

      if Text_IO.Is_Open (Session.Data.Current_File) then
         Text_IO.Close (Session.Data.Current_File);
      end if;

      --  Release Filters table

      for F in 1 .. Pattern_Action_Table.Last (Filters) loop
         Patterns.Release (Filters.Table (F).Pattern.all);
         Free (Filters.Table (F).Pattern);
         Free (Filters.Table (F).Action);
      end loop;

      for F in 1 .. File_Table.Last (Files) loop
         Free (Files.Table (F));
      end loop;

      File_Table.Set_Last (Session.Data.Files, 0);
      Field_Table.Set_Last (Session.Data.Fields, 0);
      Pattern_Action_Table.Set_Last (Session.Data.Filters, 0);

      Session.Data.NR := 0;
      Session.Data.FNR := 0;
      Session.Data.File_Index := 0;
      Session.Data.Current_Line := Null_Unbounded_String;
   end Close;

   ---------------------
   -- Current_Session --
   ---------------------

   function Current_Session return not null access Session_Type is
   begin
      return Cur_Session.Self;
   end Current_Session;

   ---------------------
   -- Default_Session --
   ---------------------

   function Default_Session return not null access Session_Type is
   begin
      return Def_Session.Self;
   end Default_Session;

   --------------------
   -- Discrete_Field --
   --------------------

   function Discrete_Field
     (Rank    : Count;
      Session : Session_Type) return Discrete
   is
   begin
      return Discrete'Value (Field (Rank, Session));
   end Discrete_Field;

   function Discrete_Field_Current_Session
     (Rank    : Count) return Discrete is
      function Do_It is new Discrete_Field (Discrete);
   begin
      return Do_It (Rank, Cur_Session);
   end Discrete_Field_Current_Session;

   -----------------
   -- End_Of_Data --
   -----------------

   function End_Of_Data
     (Session : Session_Type) return Boolean
   is
   begin
      return Session.Data.File_Index = File_Table.Last (Session.Data.Files)
        and then End_Of_File (Session);
   end End_Of_Data;

   function End_Of_Data
     return Boolean
   is
   begin
      return End_Of_Data (Cur_Session);
   end End_Of_Data;

   -----------------
   -- End_Of_File --
   -----------------

   function End_Of_File
     (Session : Session_Type) return Boolean
   is
   begin
      return Text_IO.End_Of_File (Session.Data.Current_File);
   end End_Of_File;

   function End_Of_File
     return Boolean
   is
   begin
      return End_Of_File (Cur_Session);
   end End_Of_File;

   -----------
   -- Field --
   -----------

   function Field
     (Rank    : Count;
      Session : Session_Type) return String
   is
      Fields : Field_Table.Instance renames Session.Data.Fields;

   begin
      if Rank > Number_Of_Fields (Session) then
         Raise_With_Info
           (Field_Error'Identity,
            "Field number" & Count'Image (Rank) & " does not exist.",
            Session);

      elsif Rank = 0 then

         --  Returns the whole line, this is what $0 does under Session_Type

         return To_String (Session.Data.Current_Line);

      else
         return Slice (Session.Data.Current_Line,
                       Fields.Table (Positive (Rank)).First,
                       Fields.Table (Positive (Rank)).Last);
      end if;
   end Field;

   function Field
     (Rank    : Count) return String
   is
   begin
      return Field (Rank, Cur_Session);
   end Field;

   function Field
     (Rank    : Count;
      Session : Session_Type) return Integer
   is
   begin
      return Integer'Value (Field (Rank, Session));

   exception
      when Constraint_Error =>
         Raise_With_Info
           (Field_Error'Identity,
            "Field number" & Count'Image (Rank)
            & " cannot be converted to an integer.",
            Session);
   end Field;

   function Field
     (Rank    : Count) return Integer
   is
   begin
      return Field (Rank, Cur_Session);
   end Field;

   function Field
     (Rank    : Count;
      Session : Session_Type) return Float
   is
   begin
      return Float'Value (Field (Rank, Session));

   exception
      when Constraint_Error =>
         Raise_With_Info
           (Field_Error'Identity,
            "Field number" & Count'Image (Rank)
            & " cannot be converted to a float.",
            Session);
   end Field;

   function Field
     (Rank    : Count) return Float
   is
   begin
      return Field (Rank, Cur_Session);
   end Field;

   ----------
   -- File --
   ----------

   function File
     (Session : Session_Type) return String
   is
      Files : File_Table.Instance renames Session.Data.Files;

   begin
      if Session.Data.File_Index = 0 then
         return "??";
      else
         return Files.Table (Session.Data.File_Index).all;
      end if;
   end File;

   function File
     return String
   is
   begin
      return File (Cur_Session);
   end File;

   --------------------
   -- For_Every_Line --
   --------------------

   procedure For_Every_Line
     (Separators : String        := Use_Current;
      Filename   : String        := Use_Current;
      Callbacks  : Callback_Mode := None;
      Session    : Session_Type)
   is
      Quit : Boolean;

   begin
      Open (Separators, Filename, Session);

      while not End_Of_Data (Session) loop
         Read_Line (Session);
         Split_Line (Session);

         if Callbacks in Only .. Pass_Through then
            declare
               Discard : Boolean;
            begin
               Discard := Apply_Filters (Session);
            end;
         end if;

         if Callbacks /= Only then
            Quit := False;
            Action (Quit);
            exit when Quit;
         end if;
      end loop;

      Close (Session);
   end For_Every_Line;

   procedure For_Every_Line_Current_Session
     (Separators : String        := Use_Current;
      Filename   : String        := Use_Current;
      Callbacks  : Callback_Mode := None)
   is
      procedure Do_It is new For_Every_Line (Action);
   begin
      Do_It (Separators, Filename, Callbacks, Cur_Session);
   end For_Every_Line_Current_Session;

   --------------
   -- Get_Line --
   --------------

   procedure Get_Line
     (Callbacks : Callback_Mode := None;
      Session   : Session_Type)
   is
      Filter_Active : Boolean;

   begin
      if not Text_IO.Is_Open (Session.Data.Current_File) then
         raise File_Error;
      end if;

      loop
         Read_Line (Session);
         Split_Line (Session);

         case Callbacks is
            when None =>
               exit;

            when Only =>
               Filter_Active := Apply_Filters (Session);
               exit when not Filter_Active;

            when Pass_Through =>
               Filter_Active := Apply_Filters (Session);
               exit;
         end case;
      end loop;
   end Get_Line;

   procedure Get_Line
     (Callbacks : Callback_Mode := None)
   is
   begin
      Get_Line (Callbacks, Cur_Session);
   end Get_Line;

   ----------------------
   -- Number_Of_Fields --
   ----------------------

   function Number_Of_Fields
     (Session : Session_Type) return Count
   is
   begin
      return Count (Field_Table.Last (Session.Data.Fields));
   end Number_Of_Fields;

   function Number_Of_Fields
     return Count
   is
   begin
      return Number_Of_Fields (Cur_Session);
   end Number_Of_Fields;

   --------------------------
   -- Number_Of_File_Lines --
   --------------------------

   function Number_Of_File_Lines
     (Session : Session_Type) return Count
   is
   begin
      return Count (Session.Data.FNR);
   end Number_Of_File_Lines;

   function Number_Of_File_Lines
     return Count
   is
   begin
      return Number_Of_File_Lines (Cur_Session);
   end Number_Of_File_Lines;

   ---------------------
   -- Number_Of_Files --
   ---------------------

   function Number_Of_Files
     (Session : Session_Type) return Natural
   is
      Files : File_Table.Instance renames Session.Data.Files;
   begin
      return File_Table.Last (Files);
   end Number_Of_Files;

   function Number_Of_Files
     return Natural
   is
   begin
      return Number_Of_Files (Cur_Session);
   end Number_Of_Files;

   ---------------------
   -- Number_Of_Lines --
   ---------------------

   function Number_Of_Lines
     (Session : Session_Type) return Count
   is
   begin
      return Count (Session.Data.NR);
   end Number_Of_Lines;

   function Number_Of_Lines
     return Count
   is
   begin
      return Number_Of_Lines (Cur_Session);
   end Number_Of_Lines;

   ----------
   -- Open --
   ----------

   procedure Open
     (Separators : String       := Use_Current;
      Filename   : String       := Use_Current;
      Session    : Session_Type)
   is
   begin
      if Text_IO.Is_Open (Session.Data.Current_File) then
         raise Session_Error;
      end if;

      if Filename /= Use_Current then
         File_Table.Init (Session.Data.Files);
         Add_File (Filename, Session);
      end if;

      if Separators /= Use_Current then
         Set_Field_Separators (Separators, Session);
      end if;

      Open_Next_File (Session);

   exception
      when End_Error =>
         raise File_Error;
   end Open;

   procedure Open
     (Separators : String       := Use_Current;
      Filename   : String       := Use_Current)
   is
   begin
      Open (Separators, Filename, Cur_Session);
   end Open;

   --------------------
   -- Open_Next_File --
   --------------------

   procedure Open_Next_File
     (Session : Session_Type)
   is
      Files : File_Table.Instance renames Session.Data.Files;

   begin
      if Text_IO.Is_Open (Session.Data.Current_File) then
         Text_IO.Close (Session.Data.Current_File);
      end if;

      Session.Data.File_Index := Session.Data.File_Index + 1;

      --  If there are no mores file in the table, raise End_Error

      if Session.Data.File_Index > File_Table.Last (Files) then
         raise End_Error;
      end if;

      Text_IO.Open
        (File => Session.Data.Current_File,
         Name => Files.Table (Session.Data.File_Index).all,
         Mode => Text_IO.In_File);
   end Open_Next_File;

   -----------
   -- Parse --
   -----------

   procedure Parse
     (Separators : String       := Use_Current;
      Filename   : String       := Use_Current;
      Session    : Session_Type)
   is
      Filter_Active : Boolean;
      pragma Unreferenced (Filter_Active);

   begin
      Open (Separators, Filename, Session);

      while not End_Of_Data (Session) loop
         Get_Line (None, Session);
         Filter_Active := Apply_Filters (Session);
      end loop;

      Close (Session);
   end Parse;

   procedure Parse
     (Separators : String       := Use_Current;
      Filename   : String       := Use_Current)
   is
   begin
      Parse (Separators, Filename, Cur_Session);
   end Parse;

   ---------------------
   -- Raise_With_Info --
   ---------------------

   procedure Raise_With_Info
     (E       : Exceptions.Exception_Id;
      Message : String;
      Session : Session_Type)
   is
      function Filename return String;
      --  Returns current filename and "??" if this information is not
      --  available.

      function Line return String;
      --  Returns current line number without the leading space

      --------------
      -- Filename --
      --------------

      function Filename return String is
         File : constant String := AWK.File (Session);
      begin
         if File = "" then
            return "??";
         else
            return File;
         end if;
      end Filename;

      ----------
      -- Line --
      ----------

      function Line return String is
         L : constant String := Natural'Image (Session.Data.FNR);
      begin
         return L (2 .. L'Last);
      end Line;

   --  Start of processing for Raise_With_Info

   begin
      Exceptions.Raise_Exception
        (E,
         '[' & Filename & ':' & Line & "] " & Message);
      raise Constraint_Error; -- to please GNAT as this is a No_Return proc
   end Raise_With_Info;

   ---------------
   -- Read_Line --
   ---------------

   procedure Read_Line (Session : Session_Type) is

      function Read_Line return String;
      --  Read a line in the current file. This implementation is recursive
      --  and does not have a limitation on the line length.

      NR  : Natural renames Session.Data.NR;
      FNR : Natural renames Session.Data.FNR;

      ---------------
      -- Read_Line --
      ---------------

      function Read_Line return String is
         Buffer : String (1 .. 1_024);
         Last   : Natural;

      begin
         Text_IO.Get_Line (Session.Data.Current_File, Buffer, Last);

         if Last = Buffer'Last then
            return Buffer & Read_Line;
         else
            return Buffer (1 .. Last);
         end if;
      end Read_Line;

   --  Start of processing for Read_Line

   begin
      if End_Of_File (Session) then
         Open_Next_File (Session);
         FNR := 0;
      end if;

      Session.Data.Current_Line := To_Unbounded_String (Read_Line);

      NR := NR + 1;
      FNR := FNR + 1;
   end Read_Line;

   --------------
   -- Register --
   --------------

   procedure Register
     (Field   : Count;
      Pattern : String;
      Action  : Action_Callback;
      Session : Session_Type)
   is
      Filters   : Pattern_Action_Table.Instance renames Session.Data.Filters;
      U_Pattern : constant Unbounded_String := To_Unbounded_String (Pattern);

   begin
      Pattern_Action_Table.Increment_Last (Filters);

      Filters.Table (Pattern_Action_Table.Last (Filters)) :=
        (Pattern => new Patterns.String_Pattern'(U_Pattern, Field),
         Action  => new Actions.Simple_Action'(Proc => Action));
   end Register;

   procedure Register
     (Field   : Count;
      Pattern : String;
      Action  : Action_Callback)
   is
   begin
      Register (Field, Pattern, Action, Cur_Session);
   end Register;

   procedure Register
     (Field   : Count;
      Pattern : GNAT.Regpat.Pattern_Matcher;
      Action  : Action_Callback;
      Session : Session_Type)
   is
      Filters : Pattern_Action_Table.Instance renames Session.Data.Filters;

      A_Pattern : constant Patterns.Pattern_Matcher_Access :=
                    new Regpat.Pattern_Matcher'(Pattern);
   begin
      Pattern_Action_Table.Increment_Last (Filters);

      Filters.Table (Pattern_Action_Table.Last (Filters)) :=
        (Pattern => new Patterns.Regexp_Pattern'(A_Pattern, Field),
         Action  => new Actions.Simple_Action'(Proc => Action));
   end Register;

   procedure Register
     (Field   : Count;
      Pattern : GNAT.Regpat.Pattern_Matcher;
      Action  : Action_Callback)
   is
   begin
      Register (Field, Pattern, Action, Cur_Session);
   end Register;

   procedure Register
     (Field   : Count;
      Pattern : GNAT.Regpat.Pattern_Matcher;
      Action  : Match_Action_Callback;
      Session : Session_Type)
   is
      Filters : Pattern_Action_Table.Instance renames Session.Data.Filters;

      A_Pattern : constant Patterns.Pattern_Matcher_Access :=
                    new Regpat.Pattern_Matcher'(Pattern);
   begin
      Pattern_Action_Table.Increment_Last (Filters);

      Filters.Table (Pattern_Action_Table.Last (Filters)) :=
        (Pattern => new Patterns.Regexp_Pattern'(A_Pattern, Field),
         Action  => new Actions.Match_Action'(Proc => Action));
   end Register;

   procedure Register
     (Field   : Count;
      Pattern : GNAT.Regpat.Pattern_Matcher;
      Action  : Match_Action_Callback)
   is
   begin
      Register (Field, Pattern, Action, Cur_Session);
   end Register;

   procedure Register
     (Pattern : Pattern_Callback;
      Action  : Action_Callback;
      Session : Session_Type)
   is
      Filters : Pattern_Action_Table.Instance renames Session.Data.Filters;

   begin
      Pattern_Action_Table.Increment_Last (Filters);

      Filters.Table (Pattern_Action_Table.Last (Filters)) :=
        (Pattern => new Patterns.Callback_Pattern'(Pattern => Pattern),
         Action  => new Actions.Simple_Action'(Proc => Action));
   end Register;

   procedure Register
     (Pattern : Pattern_Callback;
      Action  : Action_Callback)
   is
   begin
      Register (Pattern, Action, Cur_Session);
   end Register;

   procedure Register
     (Action  : Action_Callback;
      Session : Session_Type)
   is
   begin
      Register (Always_True'Access, Action, Session);
   end Register;

   procedure Register
     (Action  : Action_Callback)
   is
   begin
      Register (Action, Cur_Session);
   end Register;

   -----------------
   -- Set_Current --
   -----------------

   procedure Set_Current (Session : Session_Type) is
   begin
      Cur_Session.Data := Session.Data;
   end Set_Current;

   --------------------------
   -- Set_Field_Separators --
   --------------------------

   procedure Set_Field_Separators
     (Separators : String       := Default_Separators;
      Session    : Session_Type)
   is
   begin
      Free (Session.Data.Separators);

      Session.Data.Separators :=
        new Split.Separator'(Separators'Length, Separators);

      --  If there is a current line read, split it according to the new
      --  separators.

      if Session.Data.Current_Line /= Null_Unbounded_String then
         Split_Line (Session);
      end if;
   end Set_Field_Separators;

   procedure Set_Field_Separators
     (Separators : String       := Default_Separators)
   is
   begin
      Set_Field_Separators (Separators, Cur_Session);
   end Set_Field_Separators;

   ----------------------
   -- Set_Field_Widths --
   ----------------------

   procedure Set_Field_Widths
     (Field_Widths : Widths_Set;
      Session      : Session_Type)
   is
   begin
      Free (Session.Data.Separators);

      Session.Data.Separators :=
        new Split.Column'(Field_Widths'Length, Field_Widths);

      --  If there is a current line read, split it according to
      --  the new separators.

      if Session.Data.Current_Line /= Null_Unbounded_String then
         Split_Line (Session);
      end if;
   end Set_Field_Widths;

   procedure Set_Field_Widths
     (Field_Widths : Widths_Set)
   is
   begin
      Set_Field_Widths (Field_Widths, Cur_Session);
   end Set_Field_Widths;

   ----------------
   -- Split_Line --
   ----------------

   procedure Split_Line (Session : Session_Type) is
      Fields : Field_Table.Instance renames Session.Data.Fields;
   begin
      Field_Table.Init (Fields);
      Split.Current_Line (Session.Data.Separators.all, Session);
   end Split_Line;

   -------------
   -- Get_Def --
   -------------

   function Get_Def return Session_Data_Access is
   begin
      return Def_Session.Data;
   end Get_Def;

   -------------
   -- Set_Cur --
   -------------

   procedure Set_Cur is
   begin
      Cur_Session.Data := Def_Session.Data;
   end Set_Cur;

begin
   --  We have declared two sessions but both should share the same data.
   --  The current session must point to the default session as its initial
   --  value. So first we release the session data then we set current
   --  session data to point to default session data.

   Free (Cur_Session.Data);
   Cur_Session.Data := Def_Session.Data;
end GNAT.AWK;

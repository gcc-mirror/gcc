------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                            V M S _ C O N V                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1996-2005 Free Software Foundation, Inc.          --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Gnatvsn;
with Hostparm;
with Opt;
with Osint; use Osint;

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Command_Line;        use Ada.Command_Line;
with Ada.Text_IO;             use Ada.Text_IO;

package body VMS_Conv is

   Keep_Temps_Option : constant Item_Ptr :=
                         new Item'
                           (Id          => Id_Option,
                            Name        =>
                              new String'("/KEEP_TEMPORARY_FILES"),
                            Next        => null,
                            Command     => Undefined,
                            Unix_String => null);

   Param_Count : Natural := 0;
   --  Number of parameter arguments so far

   Arg_Num : Natural;
   --  Argument number

   Arg_File : Ada.Text_IO.File_Type;
   --  A file where arguments are read from

   Commands : Item_Ptr;
   --  Pointer to head of list of command items, one for each command, with
   --  the end of the list marked by a null pointer.

   Last_Command : Item_Ptr;
   --  Pointer to last item in Commands list

   Command : Item_Ptr;
   --  Pointer to command item for current command

   Make_Commands_Active : Item_Ptr := null;
   --  Set to point to Command entry for COMPILE, BIND, or LINK as appropriate
   --  if a COMMANDS_TRANSLATION switch has been encountered while processing
   --  a MAKE Command.

   Output_File_Expected : Boolean := False;
   --  True for GNAT LINK after -o switch, so that the ".ali" extension is
   --  not added to the executable file name.

   package Buffer is new Table.Table
     (Table_Component_Type => Character,
      Table_Index_Type     => Integer,
      Table_Low_Bound      => 1,
      Table_Initial        => 4096,
      Table_Increment      => 2,
      Table_Name           => "Buffer");

   function Init_Object_Dirs return Argument_List;
   --  Get the list of the object directories

   function Invert_Sense (S : String) return VMS_Data.String_Ptr;
   --  Given a unix switch string S, computes the inverse (adding or
   --  removing ! characters as required), and returns a pointer to
   --  the allocated result on the heap.

   function Is_Extensionless (F : String) return Boolean;
   --  Returns true if the filename has no extension

   function Match (S1, S2 : String) return Boolean;
   --  Determines whether S1 and S2 match (this is a case insensitive match)

   function Match_Prefix (S1, S2 : String) return Boolean;
   --  Determines whether S1 matches a prefix of S2. This is also a case
   --  insensitive match (for example Match ("AB","abc") is True).

   function Matching_Name
     (S     : String;
      Itm   : Item_Ptr;
      Quiet : Boolean := False) return Item_Ptr;
   --  Determines if the item list headed by Itm and threaded through the
   --  Next fields (with null marking the end of the list), contains an
   --  entry that uniquely matches the given string. The match is case
   --  insensitive and permits unique abbreviation. If the match succeeds,
   --  then a pointer to the matching item is returned. Otherwise, an
   --  appropriate error message is written. Note that the discriminant
   --  of Itm is used to determine the appropriate form of this message.
   --  Quiet is normally False as shown, if it is set to True, then no
   --  error message is generated in a not found situation (null is still
   --  returned to indicate the not-found situation).

   function OK_Alphanumerplus (S : String) return Boolean;
   --  Checks that S is a string of alphanumeric characters,
   --  returning True if all alphanumeric characters,
   --  False if empty or a non-alphanumeric character is present.

   function OK_Integer (S : String) return Boolean;
   --  Checks that S is a string of digits, returning True if all digits,
   --  False if empty or a non-digit is present.

   procedure Place (C : Character);
   --  Place a single character in the buffer, updating Ptr

   procedure Place (S : String);
   --  Place a string character in the buffer, updating Ptr

   procedure Place_Lower (S : String);
   --  Place string in buffer, forcing letters to lower case, updating Ptr

   procedure Place_Unix_Switches (S : VMS_Data.String_Ptr);
   --  Given a unix switch string, place corresponding switches in Buffer,
   --  updating Ptr appropriatelly. Note that in the case of use of ! the
   --  result may be to remove a previously placed switch.

   procedure Preprocess_Command_Data;
   --  Preprocess the string form of the command and options list into the
   --  internal form.

   procedure Process_Argument (The_Command : in out Command_Type);
   --  Process one argument from the command line, or one line from
   --  from a command line file. For the first call, set The_Command.

   procedure Validate_Command_Or_Option (N : VMS_Data.String_Ptr);
   --  Check that N is a valid command or option name, i.e. that it is of the
   --  form of an Ada identifier with upper case letters and underscores.

   procedure Validate_Unix_Switch (S : VMS_Data.String_Ptr);
   --  Check that S is a valid switch string as described in the syntax for
   --  the switch table item UNIX_SWITCH or else begins with a backquote.

   ----------------------
   -- Init_Object_Dirs --
   ----------------------

   function Init_Object_Dirs return Argument_List is
      Object_Dirs     : Integer;
      Object_Dir      : Argument_List (1 .. 256);
      Object_Dir_Name : String_Access;

   begin
      Object_Dirs := 0;
      Object_Dir_Name := new String'(Object_Dir_Default_Prefix);
      Get_Next_Dir_In_Path_Init (Object_Dir_Name);

      loop
         declare
            Dir : constant String_Access :=
                    String_Access (Get_Next_Dir_In_Path (Object_Dir_Name));
         begin
            exit when Dir = null;
            Object_Dirs := Object_Dirs + 1;
            Object_Dir (Object_Dirs) :=
              new String'("-L" &
                          To_Canonical_Dir_Spec
                          (To_Host_Dir_Spec
                           (Normalize_Directory_Name (Dir.all).all,
                            True).all, True).all);
         end;
      end loop;

      Object_Dirs := Object_Dirs + 1;
      Object_Dir (Object_Dirs) := new String'("-lgnat");

      if Hostparm.OpenVMS then
         Object_Dirs := Object_Dirs + 1;
         Object_Dir (Object_Dirs) := new String'("-ldecgnat");
      end if;

      return Object_Dir (1 .. Object_Dirs);
   end Init_Object_Dirs;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Command_List :=
        (Bind =>
           (Cname    => new S'("BIND"),
            Usage    => new S'("GNAT BIND file[.ali] /qualifiers"),
            VMS_Only => False,
            Unixcmd  => new S'("gnatbind"),
            Unixsws  => null,
            Switches => Bind_Switches'Access,
            Params   => new Parameter_Array'(1 => File),
            Defext   => "ali"),

         Chop =>
           (Cname    => new S'("CHOP"),
            Usage    => new S'("GNAT CHOP file [directory] /qualifiers"),
            VMS_Only => False,
            Unixcmd  => new S'("gnatchop"),
            Unixsws  => null,
            Switches => Chop_Switches'Access,
            Params   => new Parameter_Array'(1 => File, 2 => Optional_File),
            Defext   => "   "),

         Clean =>
           (Cname    => new S'("CLEAN"),
            Usage    => new S'("GNAT CLEAN /qualifiers files"),
            VMS_Only => False,
            Unixcmd  => new S'("gnatclean"),
            Unixsws  => null,
            Switches => Clean_Switches'Access,
            Params   => new Parameter_Array'(1 => File),
            Defext   => "   "),

         Compile =>
           (Cname    => new S'("COMPILE"),
            Usage    => new S'("GNAT COMPILE filespec[,...] /qualifiers"),
            VMS_Only => False,
            Unixcmd  => new S'("gnatmake"),
            Unixsws  => new Argument_List'(1 => new String'("-f"),
                                           2 => new String'("-u"),
                                           3 => new String'("-c")),
            Switches => GCC_Switches'Access,
            Params   => new Parameter_Array'(1 => Files_Or_Wildcard),
            Defext   => "   "),

         Elim =>
           (Cname    => new S'("ELIM"),
            Usage    => new S'("GNAT ELIM name /qualifiers"),
            VMS_Only => False,
            Unixcmd  => new S'("gnatelim"),
            Unixsws  => null,
            Switches => Elim_Switches'Access,
            Params   => new Parameter_Array'(1 => Other_As_Is),
            Defext   => "ali"),

         Find =>
           (Cname    => new S'("FIND"),
            Usage    => new S'("GNAT FIND pattern[:sourcefile[:line"
                               & "[:column]]] filespec[,...] /qualifiers"),
            VMS_Only => False,
            Unixcmd  => new S'("gnatfind"),
            Unixsws  => null,
            Switches => Find_Switches'Access,
            Params   => new Parameter_Array'(1 => Other_As_Is,
                                             2 => Files_Or_Wildcard),
            Defext   => "ali"),

         Krunch =>
           (Cname    => new S'("KRUNCH"),
            Usage    => new S'("GNAT KRUNCH file [/COUNT=nnn]"),
            VMS_Only => False,
            Unixcmd  => new S'("gnatkr"),
            Unixsws  => null,
            Switches => Krunch_Switches'Access,
            Params   => new Parameter_Array'(1 => File),
            Defext   => "   "),

         Library =>
           (Cname    => new S'("LIBRARY"),
            Usage    => new S'("GNAT LIBRARY /[CREATE | SET | DELETE]"
                               & "=directory [/CONFIG=file]"),
            VMS_Only => True,
            Unixcmd  => new S'("gnatlbr"),
            Unixsws  => null,
            Switches => Lbr_Switches'Access,
            Params   => new Parameter_Array'(1 .. 0 => File),
            Defext   => "   "),

         Link =>
           (Cname    => new S'("LINK"),
            Usage    => new S'("GNAT LINK file[.ali]"
                               & " [extra obj_&_lib_&_exe_&_opt files]"
                               & " /qualifiers"),
            VMS_Only => False,
            Unixcmd  => new S'("gnatlink"),
            Unixsws  => null,
            Switches => Link_Switches'Access,
            Params   => new Parameter_Array'(1 => Unlimited_Files),
            Defext   => "ali"),

         List =>
           (Cname    => new S'("LIST"),
            Usage    => new S'("GNAT LIST /qualifiers object_or_ali_file"),
            VMS_Only => False,
            Unixcmd  => new S'("gnatls"),
            Unixsws  => null,
            Switches => List_Switches'Access,
            Params   => new Parameter_Array'(1 => Unlimited_Files),
            Defext   => "ali"),

         Make =>
           (Cname    => new S'("MAKE"),
            Usage    => new S'("GNAT MAKE file(s) /qualifiers (includes "
                               & "COMPILE /qualifiers)"),
            VMS_Only => False,
            Unixcmd  => new S'("gnatmake"),
            Unixsws  => null,
            Switches => Make_Switches'Access,
            Params   => new Parameter_Array'(1 => Unlimited_Files),
            Defext   => "   "),

         Metric =>
           (Cname    => new S'("METRIC"),
            Usage    => new S'("GNAT METRIC /qualifiers source_file"),
            VMS_Only => False,
            Unixcmd  => new S'("gnatmetric"),
            Unixsws  => null,
            Switches => Metric_Switches'Access,
            Params   => new Parameter_Array'(1 => Unlimited_Files),
            Defext   => "   "),

         Name =>
           (Cname    => new S'("NAME"),
            Usage    => new S'("GNAT NAME /qualifiers naming-pattern "
                               & "[naming-patterns]"),
            VMS_Only => False,
            Unixcmd  => new S'("gnatname"),
            Unixsws  => null,
            Switches => Name_Switches'Access,
            Params   => new Parameter_Array'(1 => Unlimited_As_Is),
            Defext   => "   "),

         Preprocess =>
           (Cname    => new S'("PREPROCESS"),
            Usage    =>
              new S'("GNAT PREPROCESS ifile ofile dfile /qualifiers"),
            VMS_Only => False,
            Unixcmd  => new S'("gnatprep"),
            Unixsws  => null,
            Switches => Prep_Switches'Access,
            Params   => new Parameter_Array'(1 .. 3 => File),
            Defext   => "   "),

         Pretty =>
           (Cname    => new S'("PRETTY"),
            Usage    => new S'("GNAT PRETTY /qualifiers source_file"),
            VMS_Only => False,
            Unixcmd  => new S'("gnatpp"),
            Unixsws  => null,
            Switches => Pretty_Switches'Access,
            Params   => new Parameter_Array'(1 => Unlimited_Files),
            Defext   => "   "),

         Setup =>
           (Cname    => new S'("SETUP"),
            Usage    => new S'("GNAT SETUP /qualifiers"),
            VMS_Only => False,
            Unixcmd  => new S'(""),
            Unixsws  => null,
            Switches => Setup_Switches'Access,
            Params   => new Parameter_Array'(1 => Unlimited_Files),
            Defext   => "   "),

         Shared =>
           (Cname    => new S'("SHARED"),
            Usage    => new S'("GNAT SHARED [obj_&_lib_&_exe_&_opt"
                               & "files] /qualifiers"),
            VMS_Only => True,
            Unixcmd  => new S'("gcc"),
            Unixsws  =>
            new Argument_List'(new String'("-shared") & Init_Object_Dirs),
            Switches => Shared_Switches'Access,
            Params   => new Parameter_Array'(1 => Unlimited_Files),
            Defext   => "   "),

         Stub =>
           (Cname    => new S'("STUB"),
            Usage    => new S'("GNAT STUB file [directory]/qualifiers"),
            VMS_Only => False,
            Unixcmd  => new S'("gnatstub"),
            Unixsws  => null,
            Switches => Stub_Switches'Access,
            Params   => new Parameter_Array'(1 => File, 2 => Optional_File),
            Defext   => "   "),

         Xref =>
           (Cname    => new S'("XREF"),
            Usage    => new S'("GNAT XREF filespec[,...] /qualifiers"),
            VMS_Only => False,
            Unixcmd  => new S'("gnatxref"),
            Unixsws  => null,
            Switches => Xref_Switches'Access,
            Params   => new Parameter_Array'(1 => Files_Or_Wildcard),
            Defext   => "ali")
        );
   end Initialize;

   ------------------
   -- Invert_Sense --
   ------------------

   function Invert_Sense (S : String) return VMS_Data.String_Ptr is
      Sinv : String (1 .. S'Length * 2);
      --  Result (for sure long enough)

      Sinvp : Natural := 0;
      --  Pointer to output string

   begin
      for Sp in S'Range loop
         if Sp = S'First or else S (Sp - 1) = ',' then
            if S (Sp) = '!' then
               null;
            else
               Sinv (Sinvp + 1) := '!';
               Sinv (Sinvp + 2) := S (Sp);
               Sinvp := Sinvp + 2;
            end if;

         else
            Sinv (Sinvp + 1) := S (Sp);
            Sinvp := Sinvp + 1;
         end if;
      end loop;

      return new String'(Sinv (1 .. Sinvp));
   end Invert_Sense;

   ----------------------
   -- Is_Extensionless --
   ----------------------

   function Is_Extensionless (F : String) return Boolean is
   begin
      for J in reverse F'Range loop
         if F (J) = '.' then
            return False;
         elsif F (J) = '/' or else F (J) = ']' or else F (J) = ':' then
            return True;
         end if;
      end loop;

      return True;
   end Is_Extensionless;

   -----------
   -- Match --
   -----------

   function Match (S1, S2 : String) return Boolean is
      Dif : constant Integer := S2'First - S1'First;

   begin

      if S1'Length /= S2'Length then
         return False;

      else
         for J in S1'Range loop
            if To_Lower (S1 (J)) /= To_Lower (S2 (J + Dif)) then
               return False;
            end if;
         end loop;

         return True;
      end if;
   end Match;

   ------------------
   -- Match_Prefix --
   ------------------

   function Match_Prefix (S1, S2 : String) return Boolean is
   begin
      if S1'Length > S2'Length then
         return False;
      else
         return Match (S1, S2 (S2'First .. S2'First + S1'Length - 1));
      end if;
   end Match_Prefix;

   -------------------
   -- Matching_Name --
   -------------------

   function Matching_Name
     (S     : String;
      Itm   : Item_Ptr;
      Quiet : Boolean := False) return Item_Ptr
   is
      P1, P2 : Item_Ptr;

      procedure Err;
      --  Little procedure to output command/qualifier/option as appropriate
      --  and bump error count.

      ---------
      -- Err --
      ---------

      procedure Err is
      begin
         if Quiet then
            return;
         end if;

         Errors := Errors + 1;

         if Itm /= null then
            case Itm.Id is
               when Id_Command =>
                  Put (Standard_Error, "command");

               when Id_Switch =>
                  if Hostparm.OpenVMS then
                     Put (Standard_Error, "qualifier");
                  else
                     Put (Standard_Error, "switch");
                  end if;

               when Id_Option =>
                  Put (Standard_Error, "option");

            end case;
         else
            Put (Standard_Error, "input");

         end if;

         Put (Standard_Error, ": ");
         Put (Standard_Error, S);
      end Err;

   --  Start of processing for Matching_Name

   begin
      --  If exact match, that's the one we want

      P1 := Itm;
      while P1 /= null loop
         if Match (S, P1.Name.all) then
            return P1;
         else
            P1 := P1.Next;
         end if;
      end loop;

      --  Now check for prefix matches

      P1 := Itm;
      while P1 /= null loop
         if P1.Name.all = "/<other>" then
            return P1;

         elsif not Match_Prefix (S, P1.Name.all) then
            P1 := P1.Next;

         else
            --  Here we have found one matching prefix, so see if there is
            --  another one (which is an ambiguity)

            P2 := P1.Next;
            while P2 /= null loop
               if Match_Prefix (S, P2.Name.all) then
                  if not Quiet then
                     Put (Standard_Error, "ambiguous ");
                     Err;
                     Put (Standard_Error, " (matches ");
                     Put (Standard_Error, P1.Name.all);

                     while P2 /= null loop
                        if Match_Prefix (S, P2.Name.all) then
                           Put (Standard_Error, ',');
                           Put (Standard_Error, P2.Name.all);
                        end if;

                        P2 := P2.Next;
                     end loop;

                     Put_Line (Standard_Error, ")");
                  end if;

                  return null;
               end if;

               P2 := P2.Next;
            end loop;

            --  If we fall through that loop, then there was only one match

            return P1;
         end if;
      end loop;

      --  If we fall through outer loop, there was no match

      if not Quiet then
         Put (Standard_Error, "unrecognized ");
         Err;
         New_Line (Standard_Error);
      end if;

      return null;
   end Matching_Name;

   -----------------------
   -- OK_Alphanumerplus --
   -----------------------

   function OK_Alphanumerplus (S : String) return Boolean is
   begin
      if S'Length = 0 then
         return False;

      else
         for J in S'Range loop
            if not (Is_Alphanumeric (S (J)) or else
                    S (J) = '_' or else S (J) = '$')
            then
               return False;
            end if;
         end loop;

         return True;
      end if;
   end OK_Alphanumerplus;

   ----------------
   -- OK_Integer --
   ----------------

   function OK_Integer (S : String) return Boolean is
   begin
      if S'Length = 0 then
         return False;

      else
         for J in S'Range loop
            if not Is_Digit (S (J)) then
               return False;
            end if;
         end loop;

         return True;
      end if;
   end OK_Integer;

   --------------------
   -- Output_Version --
   --------------------

   procedure Output_Version is
   begin
      Put ("GNAT ");
      Put_Line (Gnatvsn.Gnat_Version_String);
      Put_Line ("Copyright 1996-2005 Free Software Foundation, Inc.");
   end Output_Version;

   -----------
   -- Place --
   -----------

   procedure Place (C : Character) is
   begin
      Buffer.Increment_Last;
      Buffer.Table (Buffer.Last) := C;
   end Place;

   procedure Place (S : String) is
   begin
      for J in S'Range loop
         Place (S (J));
      end loop;
   end Place;

   -----------------
   -- Place_Lower --
   -----------------

   procedure Place_Lower (S : String) is
   begin
      for J in S'Range loop
         Place (To_Lower (S (J)));
      end loop;
   end Place_Lower;

   -------------------------
   -- Place_Unix_Switches --
   -------------------------

   procedure Place_Unix_Switches (S : VMS_Data.String_Ptr) is
      P1, P2, P3 : Natural;
      Remove     : Boolean;
      Slen, Sln2 : Natural;
      Wild_Card  : Boolean := False;

   begin
      P1 := S'First;
      while P1 <= S'Last loop
         if S (P1) = '!' then
            P1 := P1 + 1;
            Remove := True;
         else
            Remove := False;
         end if;

         P2 := P1;
         pragma Assert (S (P1) = '-' or else S (P1) = '`');

         while P2 < S'Last and then S (P2 + 1) /= ',' loop
            P2 := P2 + 1;
         end loop;

         --  Switch is now in S (P1 .. P2)

         Slen := P2 - P1 + 1;

         if Remove then
            Wild_Card := S (P2) = '*';

            if Wild_Card then
               Slen := Slen - 1;
               P2   := P2 - 1;
            end if;

            P3 := 1;
            while P3 <= Buffer.Last - Slen loop
               if Buffer.Table (P3) = ' '
                 and then String (Buffer.Table (P3 + 1 .. P3 + Slen)) =
                                                             S (P1 .. P2)
                 and then (Wild_Card
                             or else
                           P3 + Slen = Buffer.Last
                             or else
                           Buffer.Table (P3 + Slen + 1) = ' ')
               then
                  Sln2 := Slen;

                  if Wild_Card then
                     while P3 + Sln2 /= Buffer.Last
                       and then Buffer.Table (P3 + Sln2 + 1) /= ' '
                     loop
                        Sln2 := Sln2 + 1;
                     end loop;
                  end if;

                  Buffer.Table (P3 .. Buffer.Last - Sln2 - 1) :=
                    Buffer.Table (P3 + Sln2 + 1 .. Buffer.Last);
                  Buffer.Set_Last (Buffer.Last - Sln2 - 1);

               else
                  P3 := P3 + 1;
               end if;
            end loop;

            if Wild_Card then
               P2 := P2 + 1;
            end if;

         else
            pragma Assert (S (P2) /= '*');
            Place (' ');

            if S (P1) = '`' then
               P1 := P1 + 1;
            end if;

            Place (S (P1 .. P2));
         end if;

         P1 := P2 + 2;
      end loop;
   end Place_Unix_Switches;

   -----------------------------
   -- Preprocess_Command_Data --
   -----------------------------

   procedure Preprocess_Command_Data is
   begin
      for C in Real_Command_Type loop
         declare
            Command : constant Item_Ptr := new Command_Item;

            Last_Switch : Item_Ptr;
            --  Last switch in list

         begin
            --  Link new command item into list of commands

            if Last_Command = null then
               Commands := Command;
            else
               Last_Command.Next := Command;
            end if;

            Last_Command := Command;

            --  Fill in fields of new command item

            Command.Name    := Command_List (C).Cname;
            Command.Usage   := Command_List (C).Usage;
            Command.Command := C;

            if Command_List (C).Unixsws = null then
               Command.Unix_String := Command_List (C).Unixcmd;
            else
               declare
                  Cmd  : String (1 .. 5_000);
                  Last : Natural := 0;
                  Sws  : constant Argument_List_Access :=
                           Command_List (C).Unixsws;

               begin
                  Cmd (1 .. Command_List (C).Unixcmd'Length) :=
                    Command_List (C).Unixcmd.all;
                  Last := Command_List (C).Unixcmd'Length;

                  for J in Sws'Range loop
                     Last := Last + 1;
                     Cmd (Last) := ' ';
                     Cmd (Last + 1 .. Last + Sws (J)'Length) :=
                       Sws (J).all;
                     Last := Last + Sws (J)'Length;
                  end loop;

                  Command.Unix_String := new String'(Cmd (1 .. Last));
               end;
            end if;

            Command.Params := Command_List (C).Params;
            Command.Defext := Command_List (C).Defext;

            Validate_Command_Or_Option (Command.Name);

            --  Process the switch list

            for S in Command_List (C).Switches'Range loop
               declare
                  SS : constant VMS_Data.String_Ptr :=
                         Command_List (C).Switches (S);
                  P  : Natural := SS'First;
                  Sw : Item_Ptr := new Switch_Item;

                  Last_Opt : Item_Ptr;
                  --  Pointer to last option

               begin
                  --  Link new switch item into list of switches

                  if Last_Switch = null then
                     Command.Switches := Sw;
                  else
                     Last_Switch.Next := Sw;
                  end if;

                  Last_Switch := Sw;

                  --  Process switch string, first get name

                  while SS (P) /= ' ' and SS (P) /= '=' loop
                     P := P + 1;
                  end loop;

                  Sw.Name := new String'(SS (SS'First .. P - 1));

                  --  Direct translation case

                  if SS (P) = ' ' then
                     Sw.Translation := T_Direct;
                     Sw.Unix_String := new String'(SS (P + 1 .. SS'Last));
                     Validate_Unix_Switch (Sw.Unix_String);

                     if SS (P - 1) = '>' then
                        Sw.Translation := T_Other;

                     elsif SS (P + 1) = '`' then
                        null;

                        --  Create the inverted case (/NO ..)

                     elsif SS (SS'First + 1 .. SS'First + 2) /= "NO" then
                        Sw := new Switch_Item;
                        Last_Switch.Next := Sw;
                        Last_Switch := Sw;

                        Sw.Name :=
                          new String'("/NO" & SS (SS'First + 1 .. P - 1));
                        Sw.Translation := T_Direct;
                        Sw.Unix_String := Invert_Sense (SS (P + 1 .. SS'Last));
                        Validate_Unix_Switch (Sw.Unix_String);
                     end if;

                  --  Directories translation case

                  elsif SS (P + 1) = '*' then
                     pragma Assert (SS (SS'Last) = '*');
                     Sw.Translation := T_Directories;
                     Sw.Unix_String := new String'(SS (P + 2 .. SS'Last - 1));
                     Validate_Unix_Switch (Sw.Unix_String);

                  --  Directory translation case

                  elsif SS (P + 1) = '%' then
                     pragma Assert (SS (SS'Last) = '%');
                     Sw.Translation := T_Directory;
                     Sw.Unix_String := new String'(SS (P + 2 .. SS'Last - 1));
                     Validate_Unix_Switch (Sw.Unix_String);

                  --  File translation case

                  elsif SS (P + 1) = '@' then
                     pragma Assert (SS (SS'Last) = '@');
                     Sw.Translation := T_File;
                     Sw.Unix_String := new String'(SS (P + 2 .. SS'Last - 1));
                     Validate_Unix_Switch (Sw.Unix_String);

                  --  No space file translation case

                  elsif SS (P + 1) = '<' then
                     pragma Assert (SS (SS'Last) = '>');
                     Sw.Translation := T_No_Space_File;
                     Sw.Unix_String := new String'(SS (P + 2 .. SS'Last - 1));
                     Validate_Unix_Switch (Sw.Unix_String);

                  --  Numeric translation case

                  elsif SS (P + 1) = '#' then
                     pragma Assert (SS (SS'Last) = '#');
                     Sw.Translation := T_Numeric;
                     Sw.Unix_String := new String'(SS (P + 2 .. SS'Last - 1));
                     Validate_Unix_Switch (Sw.Unix_String);

                  --  Alphanumerplus translation case

                  elsif SS (P + 1) = '|' then
                     pragma Assert (SS (SS'Last) = '|');
                     Sw.Translation := T_Alphanumplus;
                     Sw.Unix_String := new String'(SS (P + 2 .. SS'Last - 1));
                     Validate_Unix_Switch (Sw.Unix_String);

                  --  String translation case

                  elsif SS (P + 1) = '"' then
                     pragma Assert (SS (SS'Last) = '"');
                     Sw.Translation := T_String;
                     Sw.Unix_String := new String'(SS (P + 2 .. SS'Last - 1));
                     Validate_Unix_Switch (Sw.Unix_String);

                  --  Commands translation case

                  elsif SS (P + 1) = '?' then
                     Sw.Translation := T_Commands;
                     Sw.Unix_String := new String'(SS (P + 2 .. SS'Last));

                  --  Options translation case

                  else
                     Sw.Translation := T_Options;
                     Sw.Unix_String := new String'("");

                     P := P + 1; -- bump past =
                     while P <= SS'Last loop
                        declare
                           Opt : constant Item_Ptr := new Option_Item;
                           Q   : Natural;

                        begin
                           --  Link new option item into options list

                           if Last_Opt = null then
                              Sw.Options := Opt;
                           else
                              Last_Opt.Next := Opt;
                           end if;

                           Last_Opt := Opt;

                           --  Fill in fields of new option item

                           Q := P;
                           while SS (Q) /= ' ' loop
                              Q := Q + 1;
                           end loop;

                           Opt.Name := new String'(SS (P .. Q - 1));
                           Validate_Command_Or_Option (Opt.Name);

                           P := Q + 1;
                           Q := P;

                           while Q <= SS'Last and then SS (Q) /= ' ' loop
                              Q := Q + 1;
                           end loop;

                           Opt.Unix_String := new String'(SS (P .. Q - 1));
                           Validate_Unix_Switch (Opt.Unix_String);
                           P := Q + 1;
                        end;
                     end loop;
                  end if;
               end;
            end loop;
         end;
      end loop;
   end Preprocess_Command_Data;

   ----------------------
   -- Process_Argument --
   ----------------------

   procedure Process_Argument (The_Command : in out Command_Type) is
      Argv    : String_Access;
      Arg_Idx : Integer;

      function Get_Arg_End
        (Argv    : String;
         Arg_Idx : Integer) return Integer;
      --  Begins looking at Arg_Idx + 1 and returns the index of the
      --  last character before a slash or else the index of the last
      --  character in the string Argv.

      -----------------
      -- Get_Arg_End --
      -----------------

      function Get_Arg_End
        (Argv    : String;
         Arg_Idx : Integer) return Integer
      is
      begin
         for J in Arg_Idx + 1 .. Argv'Last loop
            if Argv (J) = '/' then
               return J - 1;
            end if;
         end loop;

         return Argv'Last;
      end Get_Arg_End;

      --  Start of processing for Process_Argument

   begin
      --  If an argument file is open, read the next non empty line

      if Is_Open (Arg_File) then
         declare
            Line : String (1 .. 256);
            Last : Natural;
         begin
            loop
               Get_Line (Arg_File, Line, Last);
               exit when Last /= 0 or else End_Of_File (Arg_File);
            end loop;

            --  If the end of the argument file has been reached, close it

            if End_Of_File (Arg_File) then
               Close (Arg_File);

               --  If the last line was empty, return after increasing Arg_Num
               --  to go to the next argument on the comment line.

               if Last = 0 then
                  Arg_Num := Arg_Num + 1;
                  return;
               end if;
            end if;

            Argv := new String'(Line (1 .. Last));
            Arg_Idx := 1;

            if Argv (1) = '@' then
               Put_Line (Standard_Error, "argument file cannot contain @cmd");
               raise Error_Exit;
            end if;
         end;

      else
         --  No argument file is open, get the argument on the command line

         Argv := new String'(Argument (Arg_Num));
         Arg_Idx := Argv'First;

         --  Check if this is the specification of an argument file

         if Argv (Arg_Idx) = '@' then
            --  The first argument on the command line cannot be an argument
            --  file.

            if Arg_Num = 1 then
               Put_Line
                 (Standard_Error,
                  "Cannot specify argument line before command");
               raise Error_Exit;
            end if;

            --  Open the file, after conversion of the name to canonical form.
            --  Fail if file is not found.

            declare
               Canonical_File_Name : String_Access :=
                 To_Canonical_File_Spec (Argv (Arg_Idx + 1 .. Argv'Last));
            begin
               Open (Arg_File, In_File, Canonical_File_Name.all);
               Free (Canonical_File_Name);
               return;

            exception
               when others =>
                  Put (Standard_Error, "Cannot open argument file """);
                  Put (Standard_Error, Argv (Arg_Idx + 1 .. Argv'Last));
                  Put_Line (Standard_Error, """");
                  raise Error_Exit;
            end;
         end if;
      end if;

      <<Tryagain_After_Coalesce>>
      loop
         declare
            Next_Arg_Idx : Integer;
            Arg          : String_Access;

         begin
            Next_Arg_Idx := Get_Arg_End (Argv.all, Arg_Idx);
            Arg := new String'(Argv (Arg_Idx .. Next_Arg_Idx));

            --  The first one must be a command name

            if Arg_Num = 1 and then Arg_Idx = Argv'First then
               Command := Matching_Name (Arg.all, Commands);

               if Command = null then
                  raise Error_Exit;
               end if;

               The_Command := Command.Command;
               Output_File_Expected := False;

               --  Give usage information if only command given

               if Argument_Count = 1
                 and then Next_Arg_Idx = Argv'Last
               then
                  Output_Version;
                  New_Line;
                  Put_Line
                    ("List of available qualifiers and options");
                  New_Line;

                  Put (Command.Usage.all);
                  Set_Col (53);
                  Put_Line (Command.Unix_String.all);

                  declare
                     Sw : Item_Ptr := Command.Switches;

                  begin
                     while Sw /= null loop
                        Put ("   ");
                        Put (Sw.Name.all);

                        case Sw.Translation is

                           when T_Other =>
                              Set_Col (53);
                              Put_Line (Sw.Unix_String.all &
                                        "/<other>");

                           when T_Direct =>
                              Set_Col (53);
                              Put_Line (Sw.Unix_String.all);

                           when T_Directories =>
                              Put ("=(direc,direc,..direc)");
                              Set_Col (53);
                              Put (Sw.Unix_String.all);
                              Put (" direc ");
                              Put (Sw.Unix_String.all);
                              Put_Line (" direc ...");

                           when T_Directory =>
                              Put ("=directory");
                              Set_Col (53);
                              Put (Sw.Unix_String.all);

                              if Sw.Unix_String (Sw.Unix_String'Last)
                              /= '='
                              then
                                 Put (' ');
                              end if;

                              Put_Line ("directory ");

                           when T_File | T_No_Space_File =>
                              Put ("=file");
                              Set_Col (53);
                              Put (Sw.Unix_String.all);

                              if Sw.Translation = T_File
                                and then Sw.Unix_String
                                  (Sw.Unix_String'Last) /= '='
                              then
                                 Put (' ');
                              end if;

                              Put_Line ("file ");

                           when T_Numeric =>
                              Put ("=nnn");
                              Set_Col (53);

                              if Sw.Unix_String
                                (Sw.Unix_String'First) = '`'
                              then
                                 Put (Sw.Unix_String
                                        (Sw.Unix_String'First + 1
                                         .. Sw.Unix_String'Last));
                              else
                                 Put (Sw.Unix_String.all);
                              end if;

                              Put_Line ("nnn");

                           when T_Alphanumplus =>
                              Put ("=xyz");
                              Set_Col (53);

                              if Sw.Unix_String
                                (Sw.Unix_String'First) = '`'
                              then
                                 Put (Sw.Unix_String
                                        (Sw.Unix_String'First + 1
                                         .. Sw.Unix_String'Last));
                              else
                                 Put (Sw.Unix_String.all);
                              end if;

                              Put_Line ("xyz");

                           when T_String =>
                              Put ("=");
                              Put ('"');
                              Put ("<string>");
                              Put ('"');
                              Set_Col (53);

                              Put (Sw.Unix_String.all);

                              if Sw.Unix_String
                                (Sw.Unix_String'Last) /= '='
                              then
                                 Put (' ');
                              end if;

                              Put ("<string>");
                              New_Line;

                           when T_Commands =>
                              Put (" (switches for ");
                              Put (Sw.Unix_String
                                     (Sw.Unix_String'First + 7
                                      .. Sw.Unix_String'Last));
                              Put (')');
                              Set_Col (53);
                              Put (Sw.Unix_String
                                     (Sw.Unix_String'First
                                      .. Sw.Unix_String'First + 5));
                              Put_Line (" switches");

                           when T_Options =>
                              declare
                                 Opt : Item_Ptr := Sw.Options;

                              begin
                                 Put_Line ("=(option,option..)");

                                 while Opt /= null loop
                                    Put ("      ");
                                    Put (Opt.Name.all);

                                    if Opt = Sw.Options then
                                       Put (" (D)");
                                    end if;

                                    Set_Col (53);
                                    Put_Line (Opt.Unix_String.all);
                                    Opt := Opt.Next;
                                 end loop;
                              end;

                        end case;

                        Sw := Sw.Next;
                     end loop;
                  end;

                  raise Normal_Exit;
               end if;

            --  Special handling for internal debugging switch /?

            elsif Arg.all = "/?" then
               Display_Command := True;
               Output_File_Expected := False;

            --  Special handling of internal option /KEEP_TEMPORARY_FILES

            elsif Arg'Length >= 7
              and then Matching_Name
                         (Arg.all, Keep_Temps_Option, True) /= null
            then
               Opt.Keep_Temporary_Files := True;

            --  Copy -switch unchanged

            elsif Arg (Arg'First) = '-' then
               Place (' ');
               Place (Arg.all);

               --  Set Output_File_Expected for the next argument

               Output_File_Expected :=
                 Arg.all = "-o" and then The_Command = Link;

               --  Copy quoted switch with quotes stripped

            elsif Arg (Arg'First) = '"' then
               if Arg (Arg'Last) /= '"' then
                  Put (Standard_Error, "misquoted argument: ");
                  Put_Line (Standard_Error, Arg.all);
                  Errors := Errors + 1;

               else
                  Place (' ');
                  Place (Arg (Arg'First + 1 .. Arg'Last - 1));
               end if;

               Output_File_Expected := False;

               --  Parameter Argument

            elsif Arg (Arg'First) /= '/'
              and then Make_Commands_Active = null
            then
               Param_Count := Param_Count + 1;

               if Param_Count <= Command.Params'Length then

                  case Command.Params (Param_Count) is

                     when File | Optional_File =>
                        declare
                           Normal_File : constant String_Access :=
                             To_Canonical_File_Spec
                               (Arg.all);

                        begin
                           Place (' ');
                           Place_Lower (Normal_File.all);

                           if Is_Extensionless (Normal_File.all)
                             and then Command.Defext /= "   "
                           then
                              Place ('.');
                              Place (Command.Defext);
                           end if;
                        end;

                     when Unlimited_Files =>
                        declare
                           Normal_File : constant String_Access :=
                             To_Canonical_File_Spec
                               (Arg.all);

                           File_Is_Wild : Boolean := False;
                           File_List    : String_Access_List_Access;

                        begin
                           for J in Arg'Range loop
                              if Arg (J) = '*'
                                or else Arg (J) = '%'
                              then
                                 File_Is_Wild := True;
                              end if;
                           end loop;

                           if File_Is_Wild then
                              File_List := To_Canonical_File_List
                                (Arg.all, False);

                              for J in File_List.all'Range loop
                                 Place (' ');
                                 Place_Lower (File_List.all (J).all);
                              end loop;

                           else
                              Place (' ');
                              Place_Lower (Normal_File.all);

                              --  Add extension if not present, except after
                              --  switch -o.

                              if Is_Extensionless (Normal_File.all)
                                and then Command.Defext /= "   "
                                and then not Output_File_Expected
                              then
                                 Place ('.');
                                 Place (Command.Defext);
                              end if;
                           end if;

                           Param_Count := Param_Count - 1;
                        end;

                     when Other_As_Is =>
                        Place (' ');
                        Place (Arg.all);

                     when Unlimited_As_Is =>
                        Place (' ');
                        Place (Arg.all);
                        Param_Count := Param_Count - 1;

                     when Files_Or_Wildcard =>

                        --  Remove spaces from a comma separated list
                        --  of file names and adjust control variables
                        --  accordingly.

                        while Arg_Num < Argument_Count and then
                          (Argv (Argv'Last) = ',' xor
                             Argument (Arg_Num + 1)
                             (Argument (Arg_Num + 1)'First) = ',')
                        loop
                           Argv := new String'
                             (Argv.all & Argument (Arg_Num + 1));
                           Arg_Num := Arg_Num + 1;
                           Arg_Idx := Argv'First;
                           Next_Arg_Idx :=
                             Get_Arg_End (Argv.all, Arg_Idx);
                           Arg := new String'
                             (Argv (Arg_Idx .. Next_Arg_Idx));
                        end loop;

                        --  Parse the comma separated list of VMS
                        --  filenames and place them on the command
                        --  line as space separated Unix style
                        --  filenames. Lower case and add default
                        --  extension as appropriate.

                        declare
                           Arg1_Idx : Integer := Arg'First;

                           function Get_Arg1_End
                             (Arg     : String;
                              Arg_Idx : Integer) return Integer;
                           --  Begins looking at Arg_Idx + 1 and
                           --  returns the index of the last character
                           --  before a comma or else the index of the
                           --  last character in the string Arg.

                           ------------------
                           -- Get_Arg1_End --
                           ------------------

                           function Get_Arg1_End
                             (Arg     : String;
                              Arg_Idx : Integer) return Integer
                           is
                           begin
                              for J in Arg_Idx + 1 .. Arg'Last loop
                                 if Arg (J) = ',' then
                                    return J - 1;
                                 end if;
                              end loop;

                              return Arg'Last;
                           end Get_Arg1_End;

                        begin
                           loop
                              declare
                                 Next_Arg1_Idx :
                                 constant Integer :=
                                   Get_Arg1_End (Arg.all, Arg1_Idx);

                                 Arg1 :
                                 constant String :=
                                   Arg (Arg1_Idx .. Next_Arg1_Idx);

                                 Normal_File :
                                 constant String_Access :=
                                   To_Canonical_File_Spec (Arg1);

                              begin
                                 Place (' ');
                                 Place_Lower (Normal_File.all);

                                 if Is_Extensionless (Normal_File.all)
                                   and then Command.Defext /= "   "
                                 then
                                    Place ('.');
                                    Place (Command.Defext);
                                 end if;

                                 Arg1_Idx := Next_Arg1_Idx + 1;
                              end;

                              exit when Arg1_Idx > Arg'Last;

                              --  Don't allow two or more commas in
                              --  a row

                              if Arg (Arg1_Idx) = ',' then
                                 Arg1_Idx := Arg1_Idx + 1;
                                 if Arg1_Idx > Arg'Last or else
                                   Arg (Arg1_Idx) = ','
                                 then
                                    Put_Line
                                      (Standard_Error,
                                       "Malformed Parameter: " &
                                       Arg.all);
                                    Put (Standard_Error, "usage: ");
                                    Put_Line (Standard_Error,
                                              Command.Usage.all);
                                    raise Error_Exit;
                                 end if;
                              end if;

                           end loop;
                        end;
                  end case;
               end if;

               --  Reset Output_File_Expected, in case it was True

               Output_File_Expected := False;

               --  Qualifier argument

            else
               Output_File_Expected := False;

               --  This code is too heavily nested, should be
               --  separated out as separate subprogram ???

               declare
                  Sw   : Item_Ptr;
                  SwP  : Natural;
                  P2   : Natural;
                  Endp : Natural := 0; -- avoid warning!
                  Opt  : Item_Ptr;

               begin
                  SwP := Arg'First;
                  while SwP < Arg'Last
                    and then Arg (SwP + 1) /= '='
                  loop
                     SwP := SwP + 1;
                  end loop;

                  --  At this point, the switch name is in
                  --  Arg (Arg'First..SwP) and if that is not the
                  --  whole switch, then there is an equal sign at
                  --  Arg (SwP + 1) and the rest of Arg is what comes
                  --  after the equal sign.

                  --  If make commands are active, see if we have
                  --  another COMMANDS_TRANSLATION switch belonging
                  --  to gnatmake.

                  if Make_Commands_Active /= null then
                     Sw :=
                       Matching_Name
                         (Arg (Arg'First .. SwP),
                          Command.Switches,
                          Quiet => True);

                     if Sw /= null
                       and then Sw.Translation = T_Commands
                     then
                        null;

                     else
                        Sw :=
                          Matching_Name
                            (Arg (Arg'First .. SwP),
                             Make_Commands_Active.Switches,
                             Quiet => False);
                     end if;

                     --  For case of GNAT MAKE or CHOP, if we cannot
                     --  find the switch, then see if it is a
                     --  recognized compiler switch instead, and if
                     --  so process the compiler switch.

                  elsif Command.Name.all = "MAKE"
                    or else Command.Name.all = "CHOP" then
                     Sw :=
                       Matching_Name
                         (Arg (Arg'First .. SwP),
                          Command.Switches,
                          Quiet => True);

                     if Sw = null then
                        Sw :=
                          Matching_Name
                            (Arg (Arg'First .. SwP),
                             Matching_Name
                               ("COMPILE", Commands).Switches,
                             Quiet => False);
                     end if;

                     --  For all other cases, just search the relevant
                     --  command.

                  else
                     Sw :=
                       Matching_Name
                         (Arg (Arg'First .. SwP),
                          Command.Switches,
                          Quiet => False);
                  end if;

                  if Sw /= null then
                     case Sw.Translation is

                        when T_Direct =>
                           Place_Unix_Switches (Sw.Unix_String);
                           if SwP < Arg'Last
                             and then Arg (SwP + 1) = '='
                           then
                              Put (Standard_Error,
                                   "qualifier options ignored: ");
                              Put_Line (Standard_Error, Arg.all);
                           end if;

                        when T_Directories =>
                           if SwP + 1 > Arg'Last then
                              Put (Standard_Error,
                                   "missing directories for: ");
                              Put_Line (Standard_Error, Arg.all);
                              Errors := Errors + 1;

                           elsif Arg (SwP + 2) /= '(' then
                              SwP := SwP + 2;
                              Endp := Arg'Last;

                           elsif Arg (Arg'Last) /= ')' then

                              --  Remove spaces from a comma separated
                              --  list of file names and adjust
                              --  control variables accordingly.

                              if Arg_Num < Argument_Count and then
                                (Argv (Argv'Last) = ',' xor
                                   Argument (Arg_Num + 1)
                                   (Argument (Arg_Num + 1)'First) = ',')
                              then
                                 Argv :=
                                   new String'(Argv.all
                                               & Argument
                                                 (Arg_Num + 1));
                                 Arg_Num := Arg_Num + 1;
                                 Arg_Idx := Argv'First;
                                 Next_Arg_Idx :=
                                   Get_Arg_End (Argv.all, Arg_Idx);
                                 Arg := new String'
                                   (Argv (Arg_Idx .. Next_Arg_Idx));
                                 goto Tryagain_After_Coalesce;
                              end if;

                              Put (Standard_Error,
                                   "incorrectly parenthesized " &
                                   "or malformed argument: ");
                              Put_Line (Standard_Error, Arg.all);
                              Errors := Errors + 1;

                           else
                              SwP := SwP + 3;
                              Endp := Arg'Last - 1;
                           end if;

                           while SwP <= Endp loop
                              declare
                                 Dir_Is_Wild       : Boolean := False;
                                 Dir_Maybe_Is_Wild : Boolean := False;

                                 Dir_List : String_Access_List_Access;

                              begin
                                 P2 := SwP;

                                 while P2 < Endp
                                   and then Arg (P2 + 1) /= ','
                                 loop
                                    --  A wildcard directory spec on
                                    --  VMS will contain either * or
                                    --  % or ...

                                    if Arg (P2) = '*' then
                                       Dir_Is_Wild := True;

                                    elsif Arg (P2) = '%' then
                                       Dir_Is_Wild := True;

                                    elsif Dir_Maybe_Is_Wild
                                      and then Arg (P2) = '.'
                                      and then Arg (P2 + 1) = '.'
                                    then
                                       Dir_Is_Wild := True;
                                       Dir_Maybe_Is_Wild := False;

                                    elsif Dir_Maybe_Is_Wild then
                                       Dir_Maybe_Is_Wild := False;

                                    elsif Arg (P2) = '.'
                                      and then Arg (P2 + 1) = '.'
                                    then
                                       Dir_Maybe_Is_Wild := True;

                                    end if;

                                    P2 := P2 + 1;
                                 end loop;

                                 if Dir_Is_Wild then
                                    Dir_List :=
                                      To_Canonical_File_List
                                        (Arg (SwP .. P2), True);

                                    for J in Dir_List.all'Range loop
                                       Place_Unix_Switches
                                         (Sw.Unix_String);
                                       Place_Lower
                                         (Dir_List.all (J).all);
                                    end loop;

                                 else
                                    Place_Unix_Switches
                                      (Sw.Unix_String);
                                    Place_Lower
                                      (To_Canonical_Dir_Spec
                                         (Arg (SwP .. P2), False).all);
                                 end if;

                                 SwP := P2 + 2;
                              end;
                           end loop;

                        when T_Directory =>
                           if SwP + 1 > Arg'Last then
                              Put (Standard_Error,
                                   "missing directory for: ");
                              Put_Line (Standard_Error, Arg.all);
                              Errors := Errors + 1;

                           else
                              Place_Unix_Switches (Sw.Unix_String);

                              --  Some switches end in "=". No space
                              --  here

                              if Sw.Unix_String
                                (Sw.Unix_String'Last) /= '='
                              then
                                 Place (' ');
                              end if;

                              Place_Lower
                                (To_Canonical_Dir_Spec
                                   (Arg (SwP + 2 .. Arg'Last),
                                    False).all);
                           end if;

                        when T_File | T_No_Space_File =>
                           if SwP + 1 > Arg'Last then
                              Put (Standard_Error,
                                   "missing file for: ");
                              Put_Line (Standard_Error, Arg.all);
                              Errors := Errors + 1;

                           else
                              Place_Unix_Switches (Sw.Unix_String);

                              --  Some switches end in "=". No space
                              --  here.

                              if Sw.Translation = T_File
                                and then Sw.Unix_String
                                  (Sw.Unix_String'Last) /= '='
                              then
                                 Place (' ');
                              end if;

                              Place_Lower
                                (To_Canonical_File_Spec
                                   (Arg (SwP + 2 .. Arg'Last)).all);
                           end if;

                        when T_Numeric =>
                           if OK_Integer (Arg (SwP + 2 .. Arg'Last)) then
                              Place_Unix_Switches (Sw.Unix_String);
                              Place (Arg (SwP + 2 .. Arg'Last));

                           else
                              Put (Standard_Error, "argument for ");
                              Put (Standard_Error, Sw.Name.all);
                              Put_Line
                                (Standard_Error, " must be numeric");
                              Errors := Errors + 1;
                           end if;

                        when T_Alphanumplus =>
                           if OK_Alphanumerplus
                             (Arg (SwP + 2 .. Arg'Last))
                           then
                              Place_Unix_Switches (Sw.Unix_String);
                              Place (Arg (SwP + 2 .. Arg'Last));

                           else
                              Put (Standard_Error, "argument for ");
                              Put (Standard_Error, Sw.Name.all);
                              Put_Line (Standard_Error,
                                        " must be alphanumeric");
                              Errors := Errors + 1;
                           end if;

                        when T_String =>

                           --  A String value must be extended to the
                           --  end of the Argv, otherwise strings like
                           --  "foo/bar" get split at the slash.

                           --  The begining and ending of the string
                           --  are flagged with embedded nulls which
                           --  are removed when building the Spawn
                           --  call. Nulls are use because they won't
                           --  show up in a /? output. Quotes aren't
                           --  used because that would make it
                           --  difficult to embed them.

                           Place_Unix_Switches (Sw.Unix_String);

                           if Next_Arg_Idx /= Argv'Last then
                              Next_Arg_Idx := Argv'Last;
                              Arg := new String'
                                (Argv (Arg_Idx .. Next_Arg_Idx));

                              SwP := Arg'First;
                              while SwP < Arg'Last and then
                              Arg (SwP + 1) /= '=' loop
                                 SwP := SwP + 1;
                              end loop;
                           end if;

                           Place (ASCII.NUL);
                           Place (Arg (SwP + 2 .. Arg'Last));
                           Place (ASCII.NUL);

                        when T_Commands =>

                           --  Output -largs/-bargs/-cargs

                           Place (' ');
                           Place (Sw.Unix_String
                                    (Sw.Unix_String'First ..
                                       Sw.Unix_String'First + 5));

                           if Sw.Unix_String
                             (Sw.Unix_String'First + 7 ..
                                Sw.Unix_String'Last) = "MAKE"
                           then
                              Make_Commands_Active := null;

                           else
                              --  Set source of new commands, also
                              --  setting this non-null indicates that
                              --  we are in the special commands mode
                              --  for processing the -xargs case.

                              Make_Commands_Active :=
                                Matching_Name
                                  (Sw.Unix_String
                                       (Sw.Unix_String'First + 7 ..
                                            Sw.Unix_String'Last),
                                   Commands);
                           end if;

                        when T_Options =>
                           if SwP + 1 > Arg'Last then
                              Place_Unix_Switches
                                (Sw.Options.Unix_String);
                              SwP := Endp + 1;

                           elsif Arg (SwP + 2) /= '(' then
                              SwP := SwP + 2;
                              Endp := Arg'Last;

                           elsif Arg (Arg'Last) /= ')' then
                              Put (Standard_Error,
                                   "incorrectly parenthesized argument: ");
                              Put_Line (Standard_Error, Arg.all);
                              Errors := Errors + 1;
                              SwP := Endp + 1;

                           else
                              SwP := SwP + 3;
                              Endp := Arg'Last - 1;
                           end if;

                           while SwP <= Endp loop
                              P2 := SwP;

                              while P2 < Endp
                                and then Arg (P2 + 1) /= ','
                              loop
                                 P2 := P2 + 1;
                              end loop;

                              --  Option name is in Arg (SwP .. P2)

                              Opt := Matching_Name (Arg (SwP .. P2),
                                                    Sw.Options);

                              if Opt /= null then
                                 Place_Unix_Switches
                                   (Opt.Unix_String);
                              end if;

                              SwP := P2 + 2;
                           end loop;

                        when T_Other =>
                           Place_Unix_Switches
                             (new String'(Sw.Unix_String.all &
                                          Arg.all));

                     end case;
                  end if;
               end;
            end if;

            Arg_Idx := Next_Arg_Idx + 1;
         end;

         exit when Arg_Idx > Argv'Last;

      end loop;

      if not Is_Open (Arg_File) then
         Arg_Num := Arg_Num + 1;
      end if;
   end Process_Argument;

   --------------------------------
   -- Validate_Command_Or_Option --
   --------------------------------

   procedure Validate_Command_Or_Option (N : VMS_Data.String_Ptr) is
   begin
      pragma Assert (N'Length > 0);

      for J in N'Range loop
         if N (J) = '_' then
            pragma Assert (N (J - 1) /= '_');
            null;
         else
            pragma Assert (Is_Upper (N (J)) or else Is_Digit (N (J)));
            null;
         end if;
      end loop;
   end Validate_Command_Or_Option;

   --------------------------
   -- Validate_Unix_Switch --
   --------------------------

   procedure Validate_Unix_Switch (S : VMS_Data.String_Ptr) is
   begin
      if S (S'First) = '`' then
         return;
      end if;

      pragma Assert (S (S'First) = '-' or else S (S'First) = '!');

      for J in S'First + 1 .. S'Last loop
         pragma Assert (S (J) /= ' ');

         if S (J) = '!' then
            pragma Assert (S (J - 1) = ',' and then S (J + 1) = '-');
            null;
         end if;
      end loop;
   end Validate_Unix_Switch;

   --------------------
   -- VMS_Conversion --
   --------------------

   procedure VMS_Conversion (The_Command : out Command_Type) is
      Result : Command_Type := Undefined;
      Result_Set : Boolean := False;
   begin
      Buffer.Init;

      --  First we must preprocess the string form of the command and options
      --  list into the internal form that we use.

      Preprocess_Command_Data;

      --  If no parameters, give complete list of commands

      if Argument_Count = 0 then
         Output_Version;
         New_Line;
         Put_Line ("List of available commands");
         New_Line;

         while Commands /= null loop
            Put (Commands.Usage.all);
            Set_Col (53);
            Put_Line (Commands.Unix_String.all);
            Commands := Commands.Next;
         end loop;

         raise Normal_Exit;
      end if;

      Arg_Num := 1;

      --  Loop through arguments

      while Arg_Num <= Argument_Count loop
         Process_Argument (Result);

         if not Result_Set then
            The_Command := Result;
            Result_Set := True;
         end if;
      end loop;

      --  Gross error checking that the number of parameters is correct.
      --  Not applicable to Unlimited_Files parameters.

      if (Param_Count = Command.Params'Length - 1
            and then Command.Params (Param_Count + 1) = Unlimited_Files)
        or else Param_Count <= Command.Params'Length
      then
         null;

      else
         Put_Line (Standard_Error,
                   "Parameter count of "
                   & Integer'Image (Param_Count)
                   & " not equal to expected "
                   & Integer'Image (Command.Params'Length));
         Put (Standard_Error, "usage: ");
         Put_Line (Standard_Error, Command.Usage.all);
         Errors := Errors + 1;
      end if;

      if Errors > 0 then
         raise Error_Exit;
      else
         --  Prepare arguments for a call to spawn, filtering out
         --  embedded nulls place there to delineate strings.

         declare
            P1, P2     : Natural;
            Inside_Nul : Boolean := False;
            Arg        : String (1 .. 1024);
            Arg_Ctr    : Natural;

         begin
            P1 := 1;

            while P1 <= Buffer.Last and then Buffer.Table (P1) = ' ' loop
               P1 := P1 + 1;
            end loop;

            Arg_Ctr := 1;
            Arg (Arg_Ctr) := Buffer.Table (P1);

            while P1 <= Buffer.Last loop

               if Buffer.Table (P1) = ASCII.NUL then
                  if Inside_Nul then
                     Inside_Nul := False;
                  else
                     Inside_Nul := True;
                  end if;
               end if;

               if Buffer.Table (P1) = ' ' and then not Inside_Nul then
                  P1 := P1 + 1;
                  Arg_Ctr := Arg_Ctr + 1;
                  Arg (Arg_Ctr) := Buffer.Table (P1);

               else
                  Last_Switches.Increment_Last;
                  P2 := P1;

                  while P2 < Buffer.Last
                    and then (Buffer.Table (P2 + 1) /= ' ' or else
                              Inside_Nul)
                  loop
                     P2 := P2 + 1;
                     Arg_Ctr := Arg_Ctr + 1;
                     Arg (Arg_Ctr) := Buffer.Table (P2);
                     if Buffer.Table (P2) = ASCII.NUL then
                        Arg_Ctr := Arg_Ctr - 1;
                        if Inside_Nul then
                           Inside_Nul := False;
                        else
                           Inside_Nul := True;
                        end if;
                     end if;
                  end loop;

                  Last_Switches.Table (Last_Switches.Last) :=
                    new String'(String (Arg (1 .. Arg_Ctr)));
                  P1 := P2 + 2;
                  Arg_Ctr := 1;
                  Arg (Arg_Ctr) := Buffer.Table (P1);
               end if;
            end loop;
         end;
      end if;
   end VMS_Conversion;

end VMS_Conv;

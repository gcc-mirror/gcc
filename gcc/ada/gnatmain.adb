------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                            G N A T M A I N                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.2 $
--                                                                          --
--          Copyright (C) 1992-2001 Free Software Foundation, Inc.          --
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

with Csets;
with GNAT.Case_Util;
with GNAT.OS_Lib;  use GNAT.OS_Lib;
with Namet;        use Namet;
with Opt;
with Osint;        use Osint;
with Output;       use Output;
with Prj;          use Prj;
with Prj.Env;
with Prj.Ext;      use Prj.Ext;
with Prj.Pars;
with Prj.Util;     use Prj.Util;
with Snames;       use Snames;
with Stringt;      use Stringt;
with Table;
with Types;        use Types;

procedure Gnatmain is

   Ada_Include_Path : constant String := "ADA_INCLUDE_PATH";
   Ada_Objects_Path : constant String := "ADA_OBJECTS_PATH";

   type Tool_Type is (None, List, Xref, Find, Stub, Make, Comp, Bind, Link);

   --  The tool that is going to be called

   Tool : Tool_Type := None;

   --  For each tool, Tool_Package_Names contains the name of the
   --  corresponding package in the project file.

   Tool_Package_Names : constant array (Tool_Type) of Name_Id :=
     (None    => No_Name,
      List    => Name_Gnatls,
      Xref    => Name_Cross_Reference,
      Find    => Name_Finder,
      Stub    => Name_Gnatstub,
      Comp    => No_Name,
      Make    => No_Name,
      Bind    => No_Name,
      Link    => No_Name);

   --  For each tool, Tool_Names contains the name of the executable
   --  to be spawned.

   Gnatmake : constant String_Access := new String'("gnatmake");

   Tool_Names : constant array (Tool_Type) of String_Access :=
     (None    => null,
      List    => new String'("gnatls"),
      Xref    => new String'("gnatxref"),
      Find    => new String'("gnatfind"),
      Stub    => new String'("gnatstub"),
      Comp    => Gnatmake,
      Make    => Gnatmake,
      Bind    => Gnatmake,
      Link    => Gnatmake);

   Project_File      : String_Access;
   Project           : Prj.Project_Id;
   Current_Verbosity : Prj.Verbosity := Prj.Default;

   --  This flag indicates a switch -p (for gnatxref and gnatfind) for
   --  an old fashioned project file. -p cannot be used in conjonction
   --  with -P.

   Old_Project_File_Used : Boolean := False;

   Next_Arg : Positive;

   --  A table to keep the switches on the command line

   package Saved_Switches is new Table.Table (
     Table_Component_Type => String_Access,
     Table_Index_Type     => Integer,
     Table_Low_Bound      => 1,
     Table_Initial        => 20,
     Table_Increment      => 100,
     Table_Name           => "Gnatmain.Saved_Switches");

   --  A table to keep the switches from the project file

   package Switches is new Table.Table (
     Table_Component_Type => String_Access,
     Table_Index_Type     => Integer,
     Table_Low_Bound      => 1,
     Table_Initial        => 20,
     Table_Increment      => 100,
     Table_Name           => "Gnatmain.Switches");

   procedure Add_Switch (Argv : String; And_Save : Boolean);
   --  Add a switch in one of the tables above

   procedure Display (Program : String; Args : Argument_List);
   --  Displays Program followed by the arguments in Args

   function Index (Char : Character; Str : String) return Natural;
   --  Returns the first occurrence of Char in Str.
   --  Returns 0 if Char is not in Str.

   procedure Scan_Arg (Argv : String; And_Save : Boolean);
   --  Scan and process arguments. Argv is a single argument.

   procedure Usage;
   --  Output usage

   ----------------
   -- Add_Switch --
   ----------------

   procedure Add_Switch (Argv : String; And_Save : Boolean) is
   begin
      if And_Save then
         Saved_Switches.Increment_Last;
         Saved_Switches.Table (Saved_Switches.Last) := new String'(Argv);

      else
         Switches.Increment_Last;
         Switches.Table (Switches.Last) := new String'(Argv);
      end if;
   end Add_Switch;

   -------------
   -- Display --
   -------------

   procedure Display (Program : String; Args : Argument_List) is
   begin
      if not Opt.Quiet_Output then
         Write_Str (Program);

         for J in Args'Range loop
            Write_Str (" ");
            Write_Str (Args (J).all);
         end loop;

         Write_Eol;
      end if;
   end Display;

   -----------
   -- Index --
   -----------

   function Index (Char : Character; Str : String) return Natural is
   begin
      for Index in Str'Range loop
         if Str (Index) = Char then
            return Index;
         end if;
      end loop;

      return 0;
   end Index;

   --------------
   -- Scan_Arg --
   --------------

   procedure Scan_Arg (Argv : String; And_Save : Boolean) is
   begin
      pragma Assert (Argv'First = 1);

      if Argv'Length = 0 then
         return;
      end if;

      if Argv (1) = Switch_Character or else Argv (1) = '-' then

         if Argv'Length = 1 then
            Fail ("switch character cannot be followed by a blank");
         end if;

         --  The two style project files (-p and -P) cannot be used together

         if (Tool = Find or else Tool = Xref)
           and then Argv (2) = 'p'
         then
            Old_Project_File_Used := True;
            if Project_File /= null then
               Fail ("-P and -p cannot be used together");
            end if;
         end if;

         --  -q Be quiet: do not output tool command

         if Argv (2 .. Argv'Last) = "q" then
            Opt.Quiet_Output := True;

            --  Only gnatstub and gnatmake have a -q switch

            if Tool = Stub or else Tool_Names (Tool) = Gnatmake then
               Add_Switch (Argv, And_Save);
            end if;

         --  gnatmake will take care of the project file related switches

         elsif Tool_Names (Tool) = Gnatmake then
            Add_Switch (Argv, And_Save);

         --  -vPx  Specify verbosity while parsing project files

         elsif Argv'Length = 4 and then Argv (2 .. 3) = "vP" then
            case Argv (4) is
               when '0' =>
                  Current_Verbosity := Prj.Default;
               when '1' =>
                  Current_Verbosity := Prj.Medium;
               when '2' =>
                  Current_Verbosity := Prj.High;
               when others =>
                  null;
            end case;

         --  -Pproject_file  Specify project file to be used

         elsif Argv'Length >= 3 and then Argv (2) = 'P' then

            --  Only one -P switch can be used

            if Project_File /= null then
               Fail (Argv & ": second project file forbidden (first is """ &
                     Project_File.all & """)");

            --  The two style project files (-p and -P) cannot be used together

            elsif Old_Project_File_Used then
               Fail ("-p and -P cannot be used together");

            else
               Project_File := new String'(Argv (3 .. Argv'Last));
            end if;

         --  -Xexternal=value Specify an external reference to be used
         --                   in project files

         elsif Argv'Length >= 5 and then Argv (2) = 'X' then
            declare
               Equal_Pos : constant Natural :=
                 Index ('=', Argv (3 .. Argv'Last));
            begin
               if Equal_Pos >= 4 and then
                  Equal_Pos /= Argv'Last then
                  Add (External_Name => Argv (3 .. Equal_Pos - 1),
                       Value => Argv (Equal_Pos + 1 .. Argv'Last));
               else
                  Fail (Argv & " is not a valid external assignment.");
               end if;
            end;

         else
            Add_Switch (Argv, And_Save);
         end if;

      else
         Add_Switch (Argv, And_Save);
      end if;

   end Scan_Arg;

   -----------
   -- Usage --
   -----------

   procedure Usage is
   begin
      Write_Str ("Usage: ");
      Write_Eol;

      Osint.Write_Program_Name;
      Write_Str ("  list  switches [list of object files]");
      Write_Eol;

      Osint.Write_Program_Name;
      Write_Str ("  xref  switches file1 file2 ...");
      Write_Eol;

      Osint.Write_Program_Name;
      Write_Str ("  find  switches pattern[:sourcefile[:line[:column]]] " &
                 "[file1 file2 ...]");
      Write_Eol;

      Osint.Write_Program_Name;
      Write_Str ("  stub  switches filename [directory]");
      Write_Eol;

      Osint.Write_Program_Name;
      Write_Str ("  comp  switches files");
      Write_Eol;

      Osint.Write_Program_Name;
      Write_Str ("  make  switches [files]");
      Write_Eol;

      Osint.Write_Program_Name;
      Write_Str ("  bind  switches files");
      Write_Eol;

      Osint.Write_Program_Name;
      Write_Str ("  link  switches files");
      Write_Eol;

      Write_Eol;

      Write_Str ("switches interpreted by ");
      Osint.Write_Program_Name;
      Write_Str (" for List Xref and Find:");
      Write_Eol;

      Write_Str ("  -q       Be quiet: do not output tool command");
      Write_Eol;

      Write_Str ("  -Pproj   Use GNAT Project File proj");
      Write_Eol;

      Write_Str ("  -vPx     Specify verbosity when parsing " &
                 "GNAT Project Files");
      Write_Eol;

      Write_Str ("  -Xnm=val Specify an external reference for " &
                 "GNAT Project Files");
      Write_Eol;

      Write_Eol;

      Write_Str ("all other arguments are transmited to the tool");
      Write_Eol;

      Write_Eol;

   end Usage;

begin

   Osint.Initialize (Unspecified);

   Namet.Initialize;
   Csets.Initialize;

   Snames.Initialize;

   Prj.Initialize;

   if Arg_Count = 1 then
      Usage;
      return;
   end if;

   --  Get the name of the tool

   declare
      Tool_Name : String (1 .. Len_Arg (1));

   begin
      Fill_Arg (Tool_Name'Address, 1);
      GNAT.Case_Util.To_Lower (Tool_Name);

      if Tool_Name = "list" then
         Tool := List;

      elsif Tool_Name = "xref" then
         Tool := Xref;

      elsif Tool_Name = "find" then
         Tool := Find;

      elsif Tool_Name = "stub" then
         Tool := Stub;

      elsif Tool_Name = "comp" then
         Tool := Comp;

      elsif Tool_Name = "make" then
         Tool := Make;

      elsif Tool_Name = "bind" then
         Tool := Bind;

      elsif Tool_Name = "link" then
         Tool := Link;

      else
         Fail ("first argument needs to be ""list"", ""xref"", ""find""" &
               ", ""stub"", ""comp"", ""make"", ""bind"" or ""link""");
      end if;
   end;

   Next_Arg := 2;

   --  Get the command line switches that follow the name of the tool

   Scan_Args : while Next_Arg < Arg_Count loop
      declare
         Next_Argv : String (1 .. Len_Arg (Next_Arg));

      begin
         Fill_Arg (Next_Argv'Address, Next_Arg);
         Scan_Arg (Next_Argv, And_Save => True);
      end;

      Next_Arg := Next_Arg + 1;
   end loop Scan_Args;

   --  If a switch -P was specified, parse the project file.
   --  Project_File is always null if we are going to invoke gnatmake,
   --  that is when Tool is Comp, Make, Bind or Link.

   if Project_File /= null then

      Prj.Pars.Set_Verbosity (To => Current_Verbosity);

      Prj.Pars.Parse
        (Project           => Project,
         Project_File_Name => Project_File.all);

      if Project = Prj.No_Project then
         Fail ("""" & Project_File.all & """ processing failed");
      end if;

      --  Check if a package with the name of the tool is in the project file
      --  and if there is one, get the switches, if any, and scan them.

      declare
         Data       : Prj.Project_Data := Prj.Projects.Table (Project);
         Pkg        : Prj.Package_Id :=
                        Prj.Util.Value_Of
                          (Name        => Tool_Package_Names (Tool),
                           In_Packages => Data.Decl.Packages);
         Element    : Package_Element;
         Default_Switches_Array : Array_Element_Id;
         Switches   : Prj.Variable_Value;
         Current    : Prj.String_List_Id;
         The_String : String_Element;

      begin
         if Pkg /= No_Package then
            Element := Packages.Table (Pkg);

            --  Packages Gnatls and Gnatstub have a single attribute Switches,
            --  that is not an associative array.

            if Tool = List or else Tool = Stub then
               Switches :=
                 Prj.Util.Value_Of
                   (Variable_Name => Name_Switches,
                    In_Variables => Element.Decl.Attributes);

               --  Packages Cross_Reference (for gnatxref) and Finder
               --  (for gnatfind) have an attributed Default_Switches,
               --  an associative array, indexed by the name of the
               --  programming language.
            else
               Default_Switches_Array :=
                 Prj.Util.Value_Of
                   (Name => Name_Default_Switches,
                    In_Arrays => Packages.Table (Pkg).Decl.Arrays);
               Switches := Prj.Util.Value_Of
                 (Index => Name_Ada,
                  In_Array => Default_Switches_Array);

            end if;

            --  If there are switches specified in the package of the
            --  project file corresponding to the tool, scan them.

            case Switches.Kind is
               when Prj.Undefined =>
                  null;

               when Prj.Single =>
                  if String_Length (Switches.Value) > 0 then
                     String_To_Name_Buffer (Switches.Value);
                     Scan_Arg
                       (Name_Buffer (1 .. Name_Len),
                        And_Save => False);
                  end if;

               when Prj.List =>
                  Current := Switches.Values;
                  while Current /= Prj.Nil_String loop
                     The_String := String_Elements.Table (Current);

                     if String_Length (The_String.Value) > 0 then
                        String_To_Name_Buffer (The_String.Value);
                        Scan_Arg
                          (Name_Buffer (1 .. Name_Len),
                           And_Save => False);
                     end if;

                     Current := The_String.Next;
                  end loop;
            end case;
         end if;
      end;

      --  Set up the environment variables ADA_INCLUDE_PATH and
      --  ADA_OBJECTS_PATH.

      Setenv
        (Name  => Ada_Include_Path,
         Value => Prj.Env.Ada_Include_Path (Project).all);
      Setenv
        (Name  => Ada_Objects_Path,
         Value => Prj.Env.Ada_Objects_Path
                       (Project, Including_Libraries => False).all);

   end if;

   --  Gather all the arguments, those from the project file first,
   --  locate the tool and call it with the arguments.

   declare
      Args    : Argument_List (1 .. Switches.Last + Saved_Switches.Last + 4);
      Arg_Num : Natural := 0;
      Tool_Path : String_Access;
      Success : Boolean;

      procedure Add (Arg : String_Access);

      procedure Add (Arg : String_Access) is
      begin
         Arg_Num := Arg_Num + 1;
         Args (Arg_Num) := Arg;
      end Add;

   begin

      case Tool is
         when Comp =>
            Add (new String'("-u"));
            Add (new String'("-f"));

         when Bind =>
            Add (new String'("-b"));

         when Link =>
            Add (new String'("-l"));

         when others =>
            null;

      end case;

      for Index in 1 .. Switches.Last loop
         Arg_Num := Arg_Num + 1;
         Args (Arg_Num) := Switches.Table (Index);
      end loop;

      for Index in 1 .. Saved_Switches.Last loop
         Arg_Num := Arg_Num + 1;
         Args (Arg_Num) := Saved_Switches.Table (Index);
      end loop;

      Tool_Path := GNAT.OS_Lib.Locate_Exec_On_Path (Tool_Names (Tool).all);

      if Tool_Path = null then
         Fail ("error, unable to locate " & Tool_Names (Tool).all);
      end if;

      Display (Tool_Names (Tool).all, Args (1 .. Arg_Num));

      GNAT.OS_Lib.Spawn (Tool_Path.all, Args (1 .. Arg_Num), Success);

   end;

end Gnatmain;

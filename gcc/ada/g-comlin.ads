------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                    G N A T . C O M M A N D _ L I N E                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 1999-2007, AdaCore                     --
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
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  High level package for command line parsing and manipulation

--  Parsing the command line
--  ========================

--  This package provides an interface for parsing command line arguments,
--  when they are either read from Ada.Command_Line or read from a string list.
--  As shown in the example below, one should first retrieve the switches
--  (special command line arguments starting with '-' by default) and their
--  parameters, and then the rest of the command line arguments.

--  This package is flexible enough to accomodate various needs: optional
--  switch parameters, various characters to separate a switch and its
--  parameter, whether to stop the parsing at the first non-switch argument
--  encountered, etc.

--  begin
--     loop
--        case Getopt ("a b: ad") is  -- Accepts '-a', '-ad', or '-b argument'
--           when ASCII.NUL => exit;

--           when 'a' =>
--                 if Full_Switch = "a" then
--                    Put_Line ("Got a");
--                 else
--                    Put_Line ("Got ad");
--                 end if;

--           when 'b' =>
--              Put_Line ("Got b + " & Parameter);

--           when others =>
--              raise Program_Error;         -- cannot occur!
--        end case;
--     end loop;

--     loop
--        declare
--           S : constant String := Get_Argument (Do_Expansion => True);
--        begin
--           exit when S'Length = 0;
--           Put_Line ("Got " & S);
--        end;
--     end loop;

--  exception
--     when Invalid_Switch    => Put_Line ("Invalid Switch " & Full_Switch);
--     when Invalid_Parameter => Put_Line ("No parameter for " & Full_Switch);
--  end;

--  A more complicated example would involve the use of sections for the
--  switches, as for instance in gnatmake. The same command line is used to
--  provide switches for several tools. Each tool recognizes its switches by
--  separating them with special switches, chosen by the programer.
--  Each section acts as a command line of its own.

--  begin
--     Initialize_Option_Scan ('-', False, "largs bargs cargs");
--     loop
--        --  Same loop as above to get switches and arguments
--     end loop;

--     Goto_Section ("bargs");
--     loop
--        --  Same loop as above to get switches and arguments
--        --  The supported switches in Get_Opt might be different
--     end loop;

--     Goto_Section ("cargs");
--     loop
--        --  Same loop as above to get switches and arguments
--        --  The supported switches in Get_Opt might be different
--     end loop;
--  end;

--  The example above have shown how to parse the command line when the
--  arguments are read directly from Ada.Command_Line. However, these arguments
--  can also be read from a list of strings. This can be useful in several
--  contexts, either because your system does not support Ada.Command_Line, or
--  because you are manipulating other tools and creating their command line by
--  hand, or for any other reason.
--  To create the list of strings, it is recommended to use
--  GNAT.OS_Lib.Argument_String_To_List.

--  The example below shows how to get the parameters from such a list. Note
--  also the use of '*' to get all the switches, and not report errors when an
--  unexpected switch was used by the user

--  declare
--     Parser : Opt_Parser;
--     Args : constant Argument_List_Access :=
--        GNAT.OS_Lib.Argument_String_To_List ("-g -O1 -Ipath");
--  begin
--     Initialize_Option_Scan (Parser, Args);
--     while Get_Opt ("* g O! I=", Parser) /= ASCII.NUL loop
--        Put_Line ("Switch " & Full_Switch (Parser)
--                  & " param=" & Parameter (Parser));
--     end loop;
--     Free (Parser);
--  end;
--
--  Creating and manipulating the command line
--  ===========================================

--  This package provides handling of command line by providing methods to
--  add or remove arguments from it. The resulting command line is kept as
--  short as possible by coalescing arguments whenever possible.

--  This package can be used to construct complex command lines for instance
--  from an GUI interface (although the package itself does not depend on a
--  specific GUI toolkit). For instance, if you are configuring the command
--  line to use when spawning a tool with the following characteristics:

--    * Specifying -gnatwa is the same as specifying -gnatwu -gnatwv, but
--      shorter and more readable

--    * All switches starting with -gnatw can be grouped, for instance one
--      can write -gnatwcd instead of -gnatwc -gnatwd.
--      Of course, this can be combined with the above and -gnatwacd is the
--      same as -gnatwc -gnatwd -gnatwu -gnatwv

--    * The switch -T is the same as -gnatwAB

--    * A switch -foo takes one mandatory parameter

--  These attributes can be configured through this package with the following
--  calls:

--     Config : Command_Line_Configuration;
--     Define_Prefix (Config, "-gnatw");
--     Define_Alias  (Config, "-gnatwa", "-gnatwuv");
--     Define_Alias  (Config, "-T",      "-gnatwAB");

--  Using this configuration, one can then construct a command line for the
--  tool with:

--     Cmd : Command_Line;
--     Set_Configuration (Cmd, Config);
--     Add_Switch (Cmd, "-bar");
--     Add_Switch (Cmd, "-gnatwu");
--     Add_Switch (Cmd, "-gnatwv");  --  will be grouped with the above
--     Add_Switch (Cmd, "-T");

--  The resulting command line can be iterated over to get all its switches,
--  There are two modes for this iteration: either you want to get the
--  shortest possible command line, which would be:

--      -bar -gnatwaAB

--  or on the other hand you want each individual switch (so that your own
--  tool does not have to do further complex processing), which would be:

--      -bar -gnatwu -gnatwv -gnatwA -gnatwB

--  Of course, we can assume that the tool you want to spawn would understand
--  both of these, since they are both compatible with the description we gave
--  above. However, the first result is useful if you want to show the user
--  what you are spawning (since that keeps the output shorter), and the second
--  output is more useful for a tool that would check whether -gnatwu was
--  passed (which isn't obvious in the first output). Likewise, the second
--  output is more useful if you have a graphical interface since each switch
--  can be associated with a widget, and you immediately know whether -gnatwu
--  was selected.
--
--  Some command line arguments can have parameters, which on a command line
--  appear as a separate argument that must immediately follow the switch.
--  Since the subprograms in this package will reorganize the switches to group
--  them, you need to indicate what is a command line
--  parameter, and what is a switch argument.

--  This is done by passing an extra argument to Add_Switch, as in:

--     Add_Switch (Cmd, "-foo", "arg1");

--  This ensures that "arg1" will always be treated as the argument to -foo,
--  and will not be grouped with other parts of the command line.

--  Parsing the command line with grouped arguments
--  ===============================================

--  This package also works great in collaboration with GNAT.Command_Line, to
--  parse the input to your tools. If you are writing the tool we described
--  above, you would do a first loop with Getopt to pass the switches and
--  their arguments, and create a temporary representation of the command line
--  as a Command_Line object. Finally, you can ask each individual switch to
--  that object. For instance:

--    declare
--      Cmd  : Command_Line;
--      Iter : Command_Line_Iterator;

--    begin
--      while Getopt ("foo: gnatw! T bar") /= ASCII.NUL loop
--         Add_Switch (Cmd, Full_Switch, Parameter);
--      end loop;

--      Start (Cmd, Iter, Expanded => True);
--      while Has_More (Iter) loop
--        if Current_Switch (Iter) = "-gnatwu" then ..
--        elsif Current_Switch (Iter) = "-gnatwv" then ...
--        end if;
--        Next (Iter);
--      end loop;

--  The above means that your tool does not have to handle on its own whether
--  the user passed -gnatwa (in which case -gnatwu was indeed selected), or
--  just -gnatwu, or a combination of -gnatw switches as in -gnatwuv.

with Ada.Command_Line;
with GNAT.Directory_Operations;
with GNAT.OS_Lib;
with GNAT.Regexp;

package GNAT.Command_Line is

   -------------
   -- Parsing --
   -------------

   type Opt_Parser is private;
   Command_Line_Parser : constant Opt_Parser;
   --  This object is responsible for parsing a list of arguments, which by
   --  default are the standard command line arguments from Ada.Command_Line.
   --  This is really a pointer to actual data, which must therefore be
   --  initialized through a call to Initialize_Option_Scan, and must be freed
   --  with a call to Free.
   --
   --  As a special case, Command_Line_Parser does not need to be either
   --  initialized or free-ed.

   procedure Initialize_Option_Scan
     (Switch_Char              : Character := '-';
      Stop_At_First_Non_Switch : Boolean := False;
      Section_Delimiters       : String := "");
   procedure Initialize_Option_Scan
     (Parser                   : out Opt_Parser;
      Command_Line             : GNAT.OS_Lib.Argument_List_Access;
      Switch_Char              : Character := '-';
      Stop_At_First_Non_Switch : Boolean := False;
      Section_Delimiters       : String := "");
   --  The first procedure resets the internal state of the package to prepare
   --  to rescan the parameters. It does not need to be called before the first
   --  use of Getopt (but it could be), but it must be called if you want to
   --  start rescanning the command line parameters from the start. The
   --  optional parameter Switch_Char can be used to reset the switch
   --  character, e.g. to '/' for use in DOS-like systems.
   --
   --  The second subprogram initializes a parser that takes its arguments from
   --  an array of strings rather than directly from the command line. In this
   --  case, the parser is responsible for freeing the strings stored in
   --  Command_Line. If you pass null to Command_Line, this will in fact create
   --  a second parser for Ada.Command_Line, which doesn't share any data with
   --  the default parser. This parser must be free-ed.
   --
   --  The optional parameter Stop_At_First_Non_Switch indicates if Getopt is
   --  to look for switches on the whole command line, or if it has to stop as
   --  soon as a non-switch argument is found.
   --
   --  Example:
   --
   --      Arguments: my_application file1 -c
   --
   --      If Stop_At_First_Non_Switch is False, then -c will be considered
   --      as a switch (returned by getopt), otherwise it will be considered
   --      as a normal argument (returned by Get_Argument).
   --
   --  If SECTION_DELIMITERS is set, then every following subprogram
   --  (Getopt and Get_Argument) will only operate within a section, which
   --  is delimited by any of these delimiters or the end of the command line.
   --
   --  Example:
   --      Initialize_Option_Scan (Section_Delimiters => "largs bargs cargs");
   --
   --      Arguments on command line : my_application -c -bargs -d -e -largs -f
   --      This line is made of three section, the first one is the default one
   --      and includes only the '-c' switch, the second one is between -bargs
   --      and -largs and includes '-d -e' and the last one includes '-f'

   procedure Free (Parser : in out Opt_Parser);
   --  Free the memory used by the parser. Calling this is not mandatory for
   --  the Command_Line_Parser

   procedure Goto_Section
     (Name   : String := "";
      Parser : Opt_Parser := Command_Line_Parser);
   --  Change the current section. The next Getopt of Get_Argument will start
   --  looking at the beginning of the section. An empty name ("") refers to
   --  the first section between the program name and the first section
   --  delimiter. If the section does not exist, then Invalid_Section is
   --  raised.

   function Full_Switch
     (Parser : Opt_Parser := Command_Line_Parser) return String;
   --  Returns the full name of the last switch found (Getopt only returns
   --  the first character)

   function Getopt
     (Switches    : String;
      Concatenate : Boolean := True;
      Parser      : Opt_Parser := Command_Line_Parser) return Character;
   --  This function moves to the next switch on the command line (defined as
   --  switch character followed by a character within Switches, casing being
   --  significant). The result returned is the first character of the switch
   --  that is located. If there are no more switches in the current section,
   --  returns ASCII.NUL. If Concatenate is True (by default), the switches
   --  does not need to be separated by spaces (they can be concatenated if
   --  they do not require an argument, e.g. -ab is the ame as two separate
   --  arguments -a -b).
   --
   --  Switches is a string of all the possible switches, separated by a
   --  space. A switch can be followed by one of the following characters:
   --
   --   ':'  The switch requires a parameter. There can optionally be a space
   --        on the command line between the switch and its parameter.
   --
   --   '='  The switch requires a parameter. There can either be a '=' or a
   --        space on the command line between the switch and its parameter.
   --
   --   '!'  The switch requires a parameter, but there can be no space on the
   --        command line between the switch and its parameter.
   --
   --   '?'  The switch may have an optional parameter. There can be no space
   --        between the switch and its argument.
   --
   --        e.g. if Switches has the following value : "a? b",
   --        The command line can be:
   --
   --             -afoo    :  -a switch with 'foo' parameter
   --             -a foo   :  -a switch and another element on the
   --                           command line 'foo', returned by Get_Argument
   --
   --     Example: if Switches is "-a: -aO:", you can have the following
   --              command lines:
   --
   --                -aarg    :  'a' switch with 'arg' parameter
   --                -a arg   :  'a' switch with 'arg' parameter
   --                -aOarg   :  'aO' switch with 'arg' parameter
   --                -aO arg  :  'aO' switch with 'arg' parameter
   --
   --    Example:
   --
   --       Getopt ("a b: ac ad?")
   --
   --         accept either 'a' or 'ac' with no argument,
   --         accept 'b' with a required argument
   --         accept 'ad' with an optional argument
   --
   --  If the first item in switches is '*', then Getopt will catch
   --  every element on the command line that was not caught by any other
   --  switch. The character returned by GetOpt is '*', but Full_Switch
   --  contains the full command line argument, including leading '-' if there
   --  is one. If this character was not returned, there would be no way of
   --  knowing whether it is there or not.
   --
   --    Example
   --       Getopt ("* a b")
   --       If the command line is '-a -c toto.o -b', Getopt will return
   --       successively 'a', '*', '*' and 'b'. When '*' is returned,
   --       Full_Switch returns the corresponding item on the command line.
   --
   --  When Getopt encounters an invalid switch, it raises the exception
   --  Invalid_Switch and sets Full_Switch to return the invalid switch.
   --  When Getopt cannot find the parameter associated with a switch, it
   --  raises Invalid_Parameter, and sets Full_Switch to return the invalid
   --  switch character.
   --
   --  Note: in case of ambiguity, e.g. switches a ab abc, then the longest
   --  matching switch is returned.
   --
   --  Arbitrary characters are allowed for switches, although it is
   --  strongly recommanded to use only letters and digits for portability
   --  reasons.
   --
   --  When Concatenate is False, individual switches need to be separated by
   --  spaces.
   --
   --    Example
   --       Getopt ("a b", Concatenate => False)
   --       If the command line is '-ab', exception Invalid_Switch will be
   --       raised and Full_Switch will return "ab".

   function Get_Argument
     (Do_Expansion : Boolean := False;
      Parser       : Opt_Parser := Command_Line_Parser) return String;
   --  Returns the next element on the command line which is not a switch.
   --  This function should not be called before Getopt has returned
   --  ASCII.NUL.
   --
   --  If Expansion is True, then the parameter on the command line will be
   --  considered as a filename with wild cards, and will be expanded. The
   --  matching file names will be returned one at a time. When there are no
   --  more arguments on the command line, this function returns an empty
   --  string. This is useful in non-Unix systems for obtaining normal
   --  expansion of wild card references.

   function Parameter
     (Parser : Opt_Parser := Command_Line_Parser) return String;
   --  Returns the parameter associated with the last switch returned by
   --  Getopt. If no parameter was associated with the last switch, or no
   --  previous call has been made to Get_Argument, raises Invalid_Parameter.
   --  If the last switch was associated with an optional argument and this
   --  argument was not found on the command line, Parameter returns an empty
   --  string.

   function Separator
     (Parser : Opt_Parser := Command_Line_Parser) return Character;
   --  The separator that was between the switch and its parameter. This is
   --  of little use in general, only if you want to know exactly what was on
   --  the command line. This is in general a single character, set to
   --  ASCII.NUL if the switch and the parameter were concatenated. A space is
   --  returned if the switch and its argument were in two separate arguments.

   type Expansion_Iterator is limited private;
   --  Type used during expansion of file names

   procedure Start_Expansion
     (Iterator     : out Expansion_Iterator;
      Pattern      : String;
      Directory    : String := "";
      Basic_Regexp : Boolean := True);
   --  Initialize a wild card expansion. The next calls to Expansion will
   --  return the next file name in Directory which match Pattern (Pattern
   --  is a regular expression, using only the Unix shell and DOS syntax if
   --  Basic_Regexp is True). When Directory is an empty string, the current
   --  directory is searched.
   --
   --  Pattern may contain directory separators (as in "src/*/*.ada").
   --  Subdirectories of Directory will also be searched, up to one
   --  hundred levels deep.
   --
   --  When Start_Expansion has been called, function Expansion should be
   --  called repeatedly until it returns an empty string, before
   --  Start_Expansion can be called again with the same Expansion_Iterator
   --  variable.

   function Expansion (Iterator : Expansion_Iterator) return String;
   --  Returns the next file in the directory matching the parameters given
   --  to Start_Expansion and updates Iterator to point to the next entry.
   --  Returns an empty string when there is no more file in the directory
   --  and its subdirectories.
   --
   --  If Expansion is called again after an empty string has been returned,
   --  then the exception GNAT.Directory_Operations.Directory_Error is raised.

   Invalid_Section : exception;
   --  Raised when an invalid section is selected by Goto_Section

   Invalid_Switch : exception;
   --  Raised when an invalid switch is detected in the command line

   Invalid_Parameter : exception;
   --  Raised when a parameter is missing, or an attempt is made to obtain a
   --  parameter for a switch that does not allow a parameter

   -----------------
   -- Configuring --
   -----------------

   type Command_Line_Configuration is private;

   procedure Define_Alias
     (Config   : in out Command_Line_Configuration;
      Switch   : String;
      Expanded : String);
   --  Indicates that whenever Switch appears on the command line, it should
   --  be expanded as Expanded. For instance, for the GNAT compiler switches,
   --  we would define "-gnatwa" as an alias for "-gnatwcfijkmopruvz", ie some
   --  default warnings to be activated.
   --
   --  Likewise, in some context you could define "--verbose" as an alias for
   --  ("-v", "--full"), ie two switches.

   procedure Define_Prefix
     (Config   : in out Command_Line_Configuration;
      Prefix   : String);
   --  Indicates that all switches starting with the given prefix should be
   --  grouped. For instance, for the GNAT compiler we would define "-gnatw"
   --  as a prefix, so that "-gnatwu -gnatwv" can be grouped into "-gnatwuv"
   --  It is assume that the remaining of the switch ("uv") is a set of
   --  characters whose order is irrelevant. In fact, this package will sort
   --  them alphabetically.

   procedure Free (Config : in out Command_Line_Configuration);
   --  Free the memory used by Config

   -------------
   -- Editing --
   -------------

   type Command_Line is private;

   procedure Set_Configuration
     (Cmd      : in out Command_Line;
      Config   : Command_Line_Configuration);
   --  Set the configuration for this command line

   procedure Set_Command_Line
     (Cmd                : in out Command_Line;
      Switches           : String;
      Getopt_Description : String    := "";
      Switch_Char        : Character := '-');
   --  Set the new content of the command line, by replacing the current
   --  version with Switches.
   --
   --  The parsing of Switches is done through calls to Getopt, by passing
   --  Getopt_Description as an argument. (a "*" is automatically prepended so
   --  that all switches and command line arguments are accepted).
   --
   --  To properly handle switches that take parameters, you should document
   --  them in Getopt_Description. Otherwise, the switch and its parameter will
   --  be recorded as two separate command line arguments as returned by a
   --  Command_Line_Iterator (which might be fine depending on your
   --  application).
   --
   --  This function can be used to reset Cmd by passing an empty string.

   procedure Add_Switch
     (Cmd       : in out Command_Line;
      Switch    : String;
      Parameter : String    := "";
      Separator : Character := ' ');
   --  Add a new switch to the command line, and combine/group it with existing
   --  switches if possible. Nothing is done if the switch already exists with
   --  the same parameter.
   --
   --  If the Switch takes a parameter, the latter should be specified
   --  separately, so that the association between the two is always correctly
   --  recognized even if the order of switches on the command line changes.
   --  For instance, you should pass "--check=full" as ("--check", "full") so
   --  that Remove_Switch below can simply take "--check" in parameter. That
   --  will automatically remove "full" as well. The value of the parameter is
   --  never modified by this package.
   --
   --  On the other hand, you could decide to simply pass "--check=full" as
   --  the Switch above, and then pass no parameter. This means that you need
   --  to pass "--check=full" to Remove_Switch as well.
   --
   --  A Switch with a parameter will never be grouped with another switch to
   --  avoid ambiguities as to who the parameter applies to.
   --
   --  Separator is the character that goes between the switches and its
   --  parameter on the command line. If it is set to ASCII.NUL, then no
   --  separator is applied, and they are concatenated

   procedure Remove_Switch
     (Cmd        : in out Command_Line;
      Switch     : String;
      Remove_All : Boolean := False);
   --  Remove Switch from the command line, and ungroup existing switches if
   --  necessary.
   --
   --  The actual parameter to the switches are ignored. If for instance
   --  you are removing "-foo", then "-foo param1" and "-foo param2" can
   --  be removed.
   --
   --  If Remove_All is True, then all matching switches are removed, otherwise
   --  only the first matching one is removed.

   procedure Remove_Switch
     (Cmd       : in out Command_Line;
      Switch    : String;
      Parameter : String);
   --  Remove a switch with a specific parameter. If Parameter is the empty
   --  string, then only a switch with no parameter will be removed.

   ---------------
   -- Iterating --
   ---------------

   type Command_Line_Iterator is private;

   procedure Start
     (Cmd      : in out Command_Line;
      Iter     : in out Command_Line_Iterator;
      Expanded : Boolean);
   --  Start iterating over the command line arguments. If Expanded is true,
   --  then the arguments are not grouped and no alias is used. For instance,
   --  "-gnatwv" and "-gnatwu" would be returned instead of "-gnatwuv".
   --
   --  The iterator becomes invalid if the command line is changed through a
   --  call to Add_Switch, Remove_Switch or Set_Command_Line.

   function Current_Switch    (Iter : Command_Line_Iterator) return String;
   function Current_Separator (Iter : Command_Line_Iterator) return String;
   function Current_Parameter (Iter : Command_Line_Iterator) return String;
   --  Return the current switch and its parameter (or the empty string if
   --  there is no parameter or the switch was added through Add_Switch
   --  without specifying the parameter.
   --
   --  Separator is the string that goes between the switch and its separator.
   --  It could be the empty string if they should be concatenated, or a space
   --  for instance. When printing, you should not add any other character.

   function Has_More (Iter : Command_Line_Iterator) return Boolean;
   --  Return True if there are more switches to be returned

   procedure Next (Iter : in out Command_Line_Iterator);
   --  Move to the next switch

   procedure Free (Cmd : in out Command_Line);
   --  Free the memory used by Cmd

private

   Max_Depth : constant := 100;
   --  Maximum depth of subdirectories

   Max_Path_Length : constant := 1024;
   --  Maximum length of relative path

   type Depth is range 1 .. Max_Depth;

   type Level is record
      Name_Last : Natural := 0;
      Dir       : GNAT.Directory_Operations.Dir_Type;
   end record;

   type Level_Array is array (Depth) of Level;

   type Section_Number is new Natural range 0 .. 65534;
   for Section_Number'Size use 16;

   type Parameter_Type is record
      Arg_Num : Positive;
      First   : Positive;
      Last    : Positive;
      Extra   : Character;
   end record;

   type Is_Switch_Type is array (Natural range <>) of Boolean;
   pragma Pack (Is_Switch_Type);

   type Section_Type is array (Natural range <>) of Section_Number;
   pragma Pack (Section_Type);

   type Expansion_Iterator is limited record
      Start : Positive := 1;
      --  Position of the first character of the relative path to check against
      --  the pattern.

      Dir_Name : String (1 .. Max_Path_Length);

      Current_Depth : Depth := 1;

      Levels : Level_Array;

      Regexp : GNAT.Regexp.Regexp;
      --  Regular expression built with the pattern

      Maximum_Depth : Depth := 1;
      --  The maximum depth of directories, reflecting the number of directory
      --  separators in the pattern.
   end record;

   type Opt_Parser_Data (Arg_Count : Natural) is record
      Arguments : GNAT.OS_Lib.Argument_List_Access;
      --  null if reading from the command line

      The_Parameter : Parameter_Type;
      The_Separator : Character;
      The_Switch    : Parameter_Type;
      --  This type and this variable are provided to store the current switch
      --  and parameter.

      Is_Switch : Is_Switch_Type (1 .. Arg_Count) := (others => False);
      --  Indicates wich arguments on the command line are considered not be
      --  switches or parameters to switches (leaving e.g. filenames,...)

      Section : Section_Type (1 .. Arg_Count) := (others => 1);
      --  Contains the number of the section associated with the current
      --  switch. If this number is 0, then it is a section delimiter, which is
      --  never returned by GetOpt.

      Current_Argument : Natural := 1;
      --  Number of the current argument parsed on the command line

      Current_Index : Natural := 1;
      --  Index in the current argument of the character to be processed

      Current_Section : Section_Number := 1;

      Expansion_It : aliased Expansion_Iterator;
      --  When Get_Argument is expanding a file name, this is the iterator used

      In_Expansion : Boolean := False;
      --  True if we are expanding a file

      Switch_Character : Character := '-';
      --  The character at the beginning of the command line arguments,
      --  indicating the beginning of a switch.

      Stop_At_First : Boolean := False;
      --  If it is True then Getopt stops at the first non-switch argument
   end record;

   Command_Line_Parser_Data : aliased Opt_Parser_Data
     (Ada.Command_Line.Argument_Count);
   --  The internal data used when parsing the command line

   type Opt_Parser is access all Opt_Parser_Data;
   Command_Line_Parser : constant Opt_Parser :=
                           Command_Line_Parser_Data'Access;

   type Command_Line_Configuration_Record is record
      Prefixes : GNAT.OS_Lib.Argument_List_Access;
      --  The list of prefixes

      Aliases    : GNAT.OS_Lib.Argument_List_Access;
      Expansions : GNAT.OS_Lib.Argument_List_Access;
      --  The aliases. Both arrays have the same indices
   end record;
   type Command_Line_Configuration is access Command_Line_Configuration_Record;

   type Command_Line is record
      Config   : Command_Line_Configuration;
      Expanded : GNAT.OS_Lib.Argument_List_Access;

      Params : GNAT.OS_Lib.Argument_List_Access;
      --  Parameter for the corresponding switch in Expanded. The first
      --  character is the separator (or ASCII.NUL if there is no separator)

      Coalesce        : GNAT.OS_Lib.Argument_List_Access;
      Coalesce_Params : GNAT.OS_Lib.Argument_List_Access;
      --  Cached version of the command line. This is recomputed every time the
      --  command line changes. Switches are grouped as much as possible, and
      --  aliases are used to reduce the length of the command line.
      --  The parameters are not allocated, they point into Params, so must not
      --  be freed.
   end record;

   type Command_Line_Iterator is record
      List     : GNAT.OS_Lib.Argument_List_Access;
      Params   : GNAT.OS_Lib.Argument_List_Access;
      Current  : Natural;
   end record;

end GNAT.Command_Line;

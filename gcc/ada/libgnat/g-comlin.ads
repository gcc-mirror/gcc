------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                    G N A T . C O M M A N D _ L I N E                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 1999-2019, AdaCore                     --
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

--  High level package for command line parsing and manipulation

----------------------------------------
-- Simple Parsing of the Command Line --
----------------------------------------

--  This package provides an interface for parsing command line arguments,
--  when they are either read from Ada.Command_Line or read from a string list.
--  As shown in the example below, one should first retrieve the switches
--  (special command line arguments starting with '-' by default) and their
--  parameters, and then the rest of the command line arguments.
--
--  While it may appear easy to parse the command line arguments with
--  Ada.Command_Line, there are in fact lots of special cases to handle in some
--  applications. Those are fully managed by GNAT.Command_Line. Among these are
--  switches with optional parameters, grouping switches (for instance "-ab"
--  might mean the same as "-a -b"), various characters to separate a switch
--  and its parameter (or none: "-a 1" and "-a1" are generally the same, which
--  can introduce confusion with grouped switches),...
--
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

--           when 'b' => Put_Line ("Got b + " & Parameter);

--           when others =>
--              raise Program_Error; -- cannot occur
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

--------------
-- Sections --
--------------

--  A more complicated example would involve the use of sections for the
--  switches, as for instance in gnatmake. The same command line is used to
--  provide switches for several tools. Each tool recognizes its switches by
--  separating them with special switches that act as section separators.
--  Each section acts as a command line of its own.

--  begin
--     Initialize_Option_Scan ('-', False, "largs bargs cargs");
--     loop
--        --  Same loop as above to get switches and arguments
--     end loop;

--     Goto_Section ("bargs");
--     loop
--        --  Same loop as above to get switches and arguments
--        --  The supported switches in Getopt might be different
--     end loop;

--     Goto_Section ("cargs");
--     loop
--        --  Same loop as above to get switches and arguments
--        --  The supported switches in Getopt might be different
--     end loop;
--  end;

-------------------------------
-- Parsing a List of Strings --
-------------------------------

--  The examples above show how to parse the command line when the arguments
--  are read directly from Ada.Command_Line. However, these arguments can also
--  be read from a list of strings. This can be useful in several contexts,
--  either because your system does not support Ada.Command_Line, or because
--  you are manipulating other tools and creating their command lines by hand,
--  or for any other reason.

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
--     while Getopt ("* g O! I=", Parser) /= ASCII.NUL loop
--        Put_Line ("Switch " & Full_Switch (Parser)
--                  & " param=" & Parameter (Parser));
--     end loop;
--     Free (Parser);
--  end;

-------------------------------------------
-- High-Level Command Line Configuration --
-------------------------------------------

--  As shown above, the code is still relatively low-level. For instance, there
--  is no way to indicate which switches are related (thus if "-l" and "--long"
--  should have the same effect, your code will need to test for both cases).
--  Likewise, it is difficult to handle more advanced constructs, like:

--    * Specifying -gnatwa is the same as specifying -gnatwu -gnatwv, but
--      shorter and more readable

--    * All switches starting with -gnatw can be grouped, for instance one
--      can write -gnatwcd instead of -gnatwc -gnatwd.
--      Of course, this can be combined with the above and -gnatwacd is the
--      same as -gnatwc -gnatwd -gnatwu -gnatwv

--    * The switch -T is the same as -gnatwAB (same as -gnatwA -gnatwB)

--  With the above form of Getopt, you would receive "-gnatwa", "-T" or
--  "-gnatwcd" in the examples above, and thus you require additional manual
--  parsing of the switch.

--  Instead, this package provides the type Command_Line_Configuration, which
--  stores all the knowledge above. For instance:

--     Config : Command_Line_Configuration;
--     Define_Alias  (Config, "-gnatwa", "-gnatwu -gnatwv");
--     Define_Prefix (Config, "-gnatw");
--     Define_Alias  (Config, "-T",      "-gnatwAB");

--  You then need to specify all possible switches in your application by
--  calling Define_Switch, for instance:

--     Define_Switch (Config, "-gnatwu", Help => "warn on unused entities");
--     Define_Switch (Config, "-gnatwv", Help => "warn on unassigned var");
--     ...

--  Specifying the help message is optional, but makes it easy to then call
--  the function:

--     Display_Help (Config);

--  that will display a properly formatted help message for your application,
--  listing all possible switches. That way you have a single place in which
--  to maintain the list of switches and their meaning, rather than maintaining
--  both the string to pass to Getopt and a subprogram to display the help.
--  Both will properly stay synchronized.

--  Once you have this Config, you just have to call:

--     Getopt (Config, Callback'Access);

--  to parse the command line. The Callback will be called for each switch
--  found on the command line (in the case of our example, that is "-gnatwu"
--  and then "-gnatwv", not "-gnatwa" itself). This simplifies command line
--  parsing a lot.

--  In fact, this can be further automated for the most command case where the
--  parameter passed to a switch is stored in a variable in the application.
--  When a switch is defined, you only have to indicate where to store the
--  value, and let Getopt do the rest. For instance:

--     Optimization : aliased Integer;
--     Verbose      : aliased Boolean;

--     Define_Switch (Config, Verbose'Access,
--                    "-v", Long_Switch => "--verbose",
--                    Help => "Output extra verbose information");
--     Define_Switch (Config, Optimization'Access,
--                    "-O?", Help => "Optimization level");

--     Getopt (Config);  --  No callback

--  Since all switches are handled automatically, we don't even need to pass
--  a callback to Getopt. Once getopt has been called, the two variables
--  Optimization and Verbose have been properly initialized, either to the
--  default value or to the value found on the command line.

------------------------------------------------
-- Creating and Manipulating the Command Line --
------------------------------------------------

--  This package provides mechanisms to create and modify command lines by
--  adding or removing arguments from them. The resulting command line is kept
--  as short as possible by coalescing arguments whenever possible.

--  Complex command lines can thus be constructed, for example from a GUI
--  (although this package does not by itself depend upon any specific GUI
--  toolkit).

--  Using the configuration defined earlier, one can then construct a command
--  line for the tool with:

--     Cmd : Command_Line;
--     Set_Configuration (Cmd, Config);   --  Config created earlier
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
--  them, you need to indicate what is a command line parameter, and what is a
--  switch argument.

--  This is done by passing an extra argument to Add_Switch, as in:

--     Add_Switch (Cmd, "-foo", Parameter => "arg1");

--  This ensures that "arg1" will always be treated as the argument to -foo,
--  and will not be grouped with other parts of the command line.

with Ada.Command_Line;

with GNAT.Directory_Operations;
with GNAT.OS_Lib;
with GNAT.Regexp;
with GNAT.Strings;

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
   --  to rescan the parameters. It does not need to be called before the
   --  first use of Getopt (but it could be), but it must be called if you
   --  want to start rescanning the command line parameters from the start.
   --  The optional parameter Switch_Char can be used to reset the switch
   --  character, e.g. to '/' for use in DOS-like systems.
   --
   --  The second subprogram initializes a parser that takes its arguments
   --  from an array of strings rather than directly from the command line. In
   --  this case, the parser is responsible for freeing the strings stored in
   --  Command_Line. If you pass null to Command_Line, this will in fact create
   --  a second parser for Ada.Command_Line, which doesn't share any data with
   --  the default parser. This parser must be free'ed.
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
   --  If Section_Delimiters is set, then every following subprogram
   --  (Getopt and Get_Argument) will only operate within a section, which
   --  is delimited by any of these delimiters or the end of the command line.
   --
   --  Example:
   --      Initialize_Option_Scan (Section_Delimiters => "largs bargs cargs");
   --
   --      Arguments on command line : my_application -c -bargs -d -e -largs -f
   --      This line contains three sections, the first one is the default one
   --      and includes only the '-c' switch, the second one is between -bargs
   --      and -largs and includes '-d -e' and the last one includes '-f'.

   procedure Free (Parser : in out Opt_Parser);
   --  Free the memory used by the parser. Calling this is not mandatory for
   --  the Command_Line_Parser

   procedure Goto_Section
     (Name   : String := "";
      Parser : Opt_Parser := Command_Line_Parser);
   --  Change the current section. The next Getopt or Get_Argument will start
   --  looking at the beginning of the section. An empty name ("") refers to
   --  the first section between the program name and the first section
   --  delimiter. If the section does not exist in Section_Delimiters, then
   --  Invalid_Section is raised. If the section does not appear on the command
   --  line, then it is treated as an empty section.

   function Full_Switch
     (Parser : Opt_Parser := Command_Line_Parser) return String;
   --  Returns the full name of the last switch found (Getopt only returns the
   --  first character). Does not include the Switch_Char ('-' by default),
   --  unless the "*" option of Getopt is used (see below).

   function Current_Section
     (Parser : Opt_Parser := Command_Line_Parser) return String;
   --  Return the name of the current section.
   --  The list of valid sections is defined through Initialize_Option_Scan

   function Getopt
     (Switches    : String;
      Concatenate : Boolean := True;
      Parser      : Opt_Parser := Command_Line_Parser) return Character;
   --  This function moves to the next switch on the command line (defined as
   --  switch character followed by a character within Switches, casing being
   --  significant). The result returned is the first character of the switch
   --  that is located. If there are no more switches in the current section,
   --  returns ASCII.NUL. If Concatenate is True (the default), the switches do
   --  not need to be separated by spaces (they can be concatenated if they do
   --  not require an argument, e.g. -ab is the same as two separate arguments
   --  -a -b).
   --
   --  Switches is a string of all the possible switches, separated by
   --  spaces. A switch can be followed by one of the following characters:
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
   --       successively 'a', '*', '*' and 'b', with Full_Switch returning
   --       "a", "-c", "toto.o", and "b".
   --
   --  When Getopt encounters an invalid switch, it raises the exception
   --  Invalid_Switch and sets Full_Switch to return the invalid switch.
   --  When Getopt cannot find the parameter associated with a switch, it
   --  raises Invalid_Parameter, and sets Full_Switch to return the invalid
   --  switch.
   --
   --  Note: in case of ambiguity, e.g. switches a ab abc, then the longest
   --  matching switch is returned.
   --
   --  Arbitrary characters are allowed for switches, although it is
   --  strongly recommended to use only letters and digits for portability
   --  reasons.
   --
   --  When Concatenate is False, individual switches need to be separated by
   --  spaces.
   --
   --    Example
   --      Getopt ("a b", Concatenate => False)
   --      If the command line is '-ab', exception Invalid_Switch will be
   --      raised and Full_Switch will return "ab".

   function Get_Argument
     (Do_Expansion : Boolean := False;
      Parser       : Opt_Parser := Command_Line_Parser) return String;
   --  Returns the next element on the command line that is not a switch. This
   --  function should be called either after Getopt has returned ASCII.NUL or
   --  after Getopt procedure call.
   --
   --  If Do_Expansion is True, then the parameter on the command line will
   --  be considered as a filename with wildcards, and will be expanded. The
   --  matching file names will be returned one at a time. This is useful in
   --  non-Unix systems for obtaining normal expansion of wildcard references.
   --  When there are no more arguments on the command line, this function
   --  returns an empty string.

   function Get_Argument
     (Do_Expansion     : Boolean    := False;
      Parser           : Opt_Parser := Command_Line_Parser;
      End_Of_Arguments : out Boolean) return String;
   --  The same as above but able to distinguish empty element in argument list
   --  from end of arguments.
   --  End_Of_Arguments is True if the end of the command line has been reached
   --  (i.e. all available arguments have been returned by previous calls to
   --  Get_Argument).

   function Parameter
     (Parser : Opt_Parser := Command_Line_Parser) return String;
   --  Returns parameter associated with the last switch returned by Getopt.
   --  If no parameter was associated with the last switch, or no previous call
   --  has been made to Get_Argument, raises Invalid_Parameter. If the last
   --  switch was associated with an optional argument and this argument was
   --  not found on the command line, Parameter returns an empty string.

   function Separator
     (Parser : Opt_Parser := Command_Line_Parser) return Character;
   --  The separator that was between the switch and its parameter. This is
   --  useful if you want to know exactly what was on the command line. This
   --  is in general a single character, set to ASCII.NUL if the switch and
   --  the parameter were concatenated. A space is returned if the switch and
   --  its argument were in two separate arguments.

   Invalid_Section : exception;
   --  Raised when an invalid section is selected by Goto_Section

   Invalid_Switch : exception;
   --  Raised when an invalid switch is detected in the command line

   Invalid_Parameter : exception;
   --  Raised when a parameter is missing, or an attempt is made to obtain a
   --  parameter for a switch that does not allow a parameter.

   -----------------------------------------
   -- Expansion of command line arguments --
   -----------------------------------------

   --  These subprograms take care of expanding globbing patterns on the
   --  command line. On Unix, such expansion is done by the shell before your
   --  application is called. But on Windows you must do this expansion
   --  yourself.

   type Expansion_Iterator is limited private;
   --  Type used during expansion of file names

   procedure Start_Expansion
     (Iterator     : out Expansion_Iterator;
      Pattern      : String;
      Directory    : String := "";
      Basic_Regexp : Boolean := True);
   --  Initialize a wildcard expansion. The next calls to Expansion will
   --  return the next file name in Directory which match Pattern (Pattern
   --  is a regular expression, using only the Unix shell and DOS syntax if
   --  Basic_Regexp is True). When Directory is an empty string, the current
   --  directory is searched.
   --
   --  Pattern may contain directory separators (as in "src/*/*.ada").
   --  Subdirectories of Directory will also be searched, up to one
   --  hundred levels deep.
   --
   --  When Start_Expansion has been called, function Expansion should
   --  be called repeatedly until it returns an empty string, before
   --  Start_Expansion can be called again with the same Expansion_Iterator
   --  variable.

   function Expansion (Iterator : Expansion_Iterator) return String;
   --  Returns the next file in the directory matching the parameters given
   --  to Start_Expansion and updates Iterator to point to the next entry.
   --  Returns an empty string when there are no more files.
   --
   --  If Expansion is called again after an empty string has been returned,
   --  then the exception GNAT.Directory_Operations.Directory_Error is raised.

   -----------------
   -- Configuring --
   -----------------

   --  The following subprograms are used to manipulate a command line
   --  represented as a string (for instance "-g -O2"), as well as parsing
   --  the switches from such a string. They provide high-level configurations
   --  to define aliases (a switch is equivalent to one or more other switches)
   --  or grouping of switches ("-gnatyac" is equivalent to "-gnatya" and
   --  "-gnatyc").

   --  See the top of this file for examples on how to use these subprograms

   type Command_Line_Configuration is private;

   procedure Define_Section
     (Config  : in out Command_Line_Configuration;
      Section : String);
   --  Indicates a new switch section. All switches belonging to the same
   --  section are ordered together, preceded by the section. They are placed
   --  at the end of the command line (as in "gnatmake somefile.adb -cargs -g")
   --
   --  The section name should not include the leading '-'. So for instance in
   --  the case of gnatmake we would use:
   --
   --    Define_Section (Config, "cargs");
   --    Define_Section (Config, "bargs");

   procedure Define_Alias
     (Config   : in out Command_Line_Configuration;
      Switch   : String;
      Expanded : String;
      Section  : String := "");
   --  Indicates that whenever Switch appears on the command line, it should
   --  be expanded as Expanded. For instance, for the GNAT compiler switches,
   --  we would define "-gnatwa" as an alias for "-gnatwcfijkmopruvz", ie some
   --  default warnings to be activated.
   --
   --  This expansion is only done within the specified section, which must
   --  have been defined first through a call to [Define_Section].

   procedure Define_Prefix
     (Config : in out Command_Line_Configuration;
      Prefix : String);
   --  Indicates that all switches starting with the given prefix should be
   --  grouped. For instance, for the GNAT compiler we would define "-gnatw" as
   --  a prefix, so that "-gnatwu -gnatwv" can be grouped into "-gnatwuv" It is
   --  assumed that the remainder of the switch ("uv") is a set of characters
   --  whose order is irrelevant. In fact, this package will sort them
   --  alphabetically.
   --
   --  When grouping switches that accept arguments (for instance "-gnatyL!"
   --  as the definition, and "-gnatyaL12b" as the command line), only
   --  numerical arguments are accepted. The above is equivalent to
   --  "-gnatya -gnatyL12 -gnatyb".

   procedure Define_Switch
     (Config      : in out Command_Line_Configuration;
      Switch      : String := "";
      Long_Switch : String := "";
      Help        : String := "";
      Section     : String := "";
      Argument    : String := "ARG");
   --  Indicates a new switch. The format of this switch follows the getopt
   --  format (trailing ':', '?', etc for defining a switch with parameters).
   --
   --  Switch should also start with the leading '-' (or any other characters).
   --  If this character is not '-', you need to call Initialize_Option_Scan to
   --  set the proper character for the parser.
   --
   --  The switches defined in the command_line_configuration object are used
   --  when ungrouping switches with more that one character after the prefix.
   --
   --  Switch and Long_Switch (when specified) are aliases and can be used
   --  interchangeably. There is no check that they both take an argument or
   --  both take no argument. Switch can be set to "*" to indicate that any
   --  switch is supported (in which case Getopt will return '*', see its
   --  documentation).
   --
   --  Help is used by the Display_Help procedure to describe the supported
   --  switches.
   --
   --  In_Section indicates in which section the switch is valid (you need to
   --  first define the section through a call to Define_Section).
   --
   --  Argument is the name of the argument, as displayed in the automatic
   --  help message. It is always capitalized for consistency.

   procedure Define_Switch
     (Config      : in out Command_Line_Configuration;
      Output      : access Boolean;
      Switch      : String := "";
      Long_Switch : String := "";
      Help        : String := "";
      Section     : String := "";
      Value       : Boolean := True);
   --  See Define_Switch for a description of the parameters.
   --  When the switch is found on the command line, Getopt will set
   --  Output.all to Value.
   --
   --  Output is always initially set to "not Value", so that if the switch is
   --  not found on the command line, Output still has a valid value.
   --  The switch must not take any parameter.
   --
   --  Output must exist at least as long as Config, otherwise an erroneous
   --  memory access may occur.

   procedure Define_Switch
     (Config      : in out Command_Line_Configuration;
      Output      : access Integer;
      Switch      : String := "";
      Long_Switch : String := "";
      Help        : String := "";
      Section     : String := "";
      Initial     : Integer := 0;
      Default     : Integer := 1;
      Argument    : String := "ARG");
   --  See Define_Switch for a description of the parameters. When the
   --  switch is found on the command line, Getopt will set Output.all to the
   --  value of the switch's parameter. If the parameter is not an integer,
   --  Invalid_Parameter is raised.

   --  Output is always initialized to Initial. If the switch has an optional
   --  argument which isn't specified by the user, then Output will be set to
   --  Default. The switch must accept an argument.

   procedure Define_Switch
     (Config      : in out Command_Line_Configuration;
      Output      : access GNAT.Strings.String_Access;
      Switch      : String := "";
      Long_Switch : String := "";
      Help        : String := "";
      Section     : String := "";
      Argument    : String := "ARG");
   --  Set Output to the value of the switch's parameter when the switch is
   --  found on the command line. Output is always initialized to the empty
   --  string if it does not have a value already (otherwise it is left as is
   --  so that you can specify the default value directly in the declaration
   --  of the variable). The switch must accept an argument.

   type Value_Callback is access procedure (Switch, Value : String);

   procedure Define_Switch
     (Config      : in out Command_Line_Configuration;
      Callback    : not null Value_Callback;
      Switch      : String := "";
      Long_Switch : String := "";
      Help        : String := "";
      Section     : String := "";
      Argument    : String := "ARG");
   --  Call Callback for each instance of Switch. The callback is given the
   --  actual switch and the corresponding value. The switch must accept
   --  an argument.

   procedure Set_Usage
     (Config   : in out Command_Line_Configuration;
      Usage    : String := "[switches] [arguments]";
      Help     : String := "";
      Help_Msg : String := "");
   --  Defines the general format of the call to the application, and a short
   --  help text. These are both displayed by Display_Help. When a non-empty
   --  Help_Msg is given, it is used by Display_Help instead of the
   --  automatically generated list of supported switches.

   procedure Display_Help (Config : Command_Line_Configuration);
   --  Display the help for the tool (i.e. its usage, and its supported
   --  switches).

   function Get_Switches
     (Config      : Command_Line_Configuration;
      Switch_Char : Character := '-';
      Section     : String := "") return String;
   --  Get the switches list as expected by Getopt, for a specific section of
   --  the command line. This list is built using all switches defined
   --  previously via Define_Switch above.

   function Section_Delimiters
     (Config : Command_Line_Configuration) return String;
   --  Return a string suitable for use in Initialize_Option_Scan

   procedure Free (Config : in out Command_Line_Configuration);
   --  Free the memory used by Config

   type Switch_Handler is access procedure
     (Switch    : String;
      Parameter : String;
      Section   : String);
   --  Called when a switch is found on the command line. Switch includes
   --  any leading '-' that was specified in Define_Switch. This is slightly
   --  different from the functional version of Getopt above, for which
   --  Full_Switch omits the first leading '-'.

   Exit_From_Command_Line : exception;
   --  Raised when the program should exit because Getopt below has seen
   --  a -h or --help switch.

   procedure Getopt
     (Config      : Command_Line_Configuration;
      Callback    : Switch_Handler := null;
      Parser      : Opt_Parser := Command_Line_Parser;
      Concatenate : Boolean := True;
      Quiet       : Boolean := False);
   --  Similar to the standard Getopt function. For each switch found on the
   --  command line, this calls Callback, if the switch is not handled
   --  automatically.
   --
   --  The list of valid switches are the ones from the configuration. The
   --  switches that were declared through Define_Switch with an Output
   --  parameter are never returned (and result in a modification of the Output
   --  variable). This function will in fact never call [Callback] if all
   --  switches were handled automatically and there is nothing left to do.
   --
   --  The option Concatenate is identical to the one of the standard Getopt
   --  function.
   --
   --  This procedure automatically adds -h and --help to the valid switches,
   --  to display the help message and raises Exit_From_Command_Line.
   --  If an invalid switch is specified on the command line, this procedure
   --  will display an error message and raises Invalid_Switch again.
   --  If the Quiet parameter is True then the error message is not displayed.
   --
   --  This function automatically expands switches:
   --
   --    If Define_Prefix was called (for instance "-gnaty") and the user
   --    specifies "-gnatycb" on the command line, then Getopt returns
   --    "-gnatyc" and "-gnatyb" separately.
   --
   --    If Define_Alias was called (for instance "-gnatya = -gnatycb") then
   --    the latter is returned (in this case it also expands -gnaty as per
   --    the above.
   --
   --  The goal is to make handling as easy as possible by leaving as much
   --  work as possible to this package.
   --
   --  As opposed to the standard Getopt, this one will analyze all sections
   --  as defined by Define_Section, and automatically jump from one section to
   --  the next.

   ------------------------------
   -- Generating command lines --
   ------------------------------

   --  Once the command line configuration has been created, you can build your
   --  own command line. This will be done in general because you need to spawn
   --  external tools from your application.

   --  Although it could be done by concatenating strings, the following
   --  subprograms will properly take care of grouping switches when possible,
   --  so as to keep the command line as short as possible. They also provide a
   --  way to remove a switch from an existing command line.

   --  For instance:

   --      declare
   --         Config : Command_Line_Configuration;
   --         Line : Command_Line;
   --         Args : Argument_List_Access;

   --      begin
   --         Define_Switch (Config, "-gnatyc");
   --         Define_Switch (Config, ...);  --  for all valid switches
   --         Define_Prefix (Config, "-gnaty");

   --         Set_Configuration (Line, Config);
   --         Add_Switch (Line, "-O2");
   --         Add_Switch (Line, "-gnatyc");
   --         Add_Switch (Line, "-gnatyd");
   --
   --         Build (Line, Args);
   --         --   Args is now  ["-O2", "-gnatycd"]
   --      end;

   type Command_Line is private;

   procedure Set_Configuration
     (Cmd    : in out Command_Line;
      Config : Command_Line_Configuration);
   function Get_Configuration
     (Cmd : Command_Line) return Command_Line_Configuration;
   --  Set or retrieve the configuration used for that command line. The Config
   --  must have been initialized first, by calling one of the Define_Switches
   --  subprograms.

   procedure Set_Command_Line
     (Cmd                : in out Command_Line;
      Switches           : String;
      Getopt_Description : String    := "";
      Switch_Char        : Character := '-');
   --  Set the new content of the command line, by replacing the current
   --  version with Switches.
   --
   --  The parsing of Switches is done through calls to Getopt, by passing
   --  Getopt_Description as an argument. (A "*" is automatically prepended so
   --  that all switches and command line arguments are accepted). If a config
   --  was defined via Set_Configuration, the Getopt_Description parameter will
   --  be ignored.
   --
   --  To properly handle switches that take parameters, you should document
   --  them in Getopt_Description. Otherwise, the switch and its parameter will
   --  be recorded as two separate command line arguments as returned by a
   --  Command_Line_Iterator (which might be fine depending on your
   --  application).
   --
   --  If the command line has sections (such as -bargs -cargs), then they
   --  should be listed in the Sections parameter (as "-bargs -cargs").
   --
   --  This function can be used to reset Cmd by passing an empty string
   --
   --  If an invalid switch is found on the command line (i.e. wasn't defined
   --  in the configuration via Define_Switch), and the configuration wasn't
   --  set to accept all switches (by defining "*" as a valid switch), then an
   --  exception Invalid_Switch is raised. The exception message indicates the
   --  invalid switch.

   procedure Add_Switch
     (Cmd        : in out Command_Line;
      Switch     : String;
      Parameter  : String    := "";
      Separator  : Character := ASCII.NUL;
      Section    : String    := "";
      Add_Before : Boolean   := False);
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
   --  avoid ambiguities as to what the parameter applies to.
   --
   --  If the switch is part of a section, then it should be specified so that
   --  the switch is correctly placed in the command line, and the section
   --  added if not already present. For example, to add the -g switch into the
   --  -cargs section, you need to call (Cmd, "-g", Section => "-cargs").
   --
   --  [Separator], if specified, overrides the separator that was defined
   --  through Define_Switch. For instance, if the switch was defined as
   --  "-from:", the separator defaults to a space. But if your application
   --  uses unusual separators not supported by GNAT.Command_Line (for instance
   --  it requires ":"), you can specify this separator here.
   --
   --  For instance,
   --     Add_Switch(Cmd, "-from", "bar", ':')
   --
   --  results in
   --     -from:bar
   --
   --  rather than the default
   --     -from bar
   --
   --  Note however that Getopt doesn't know how to handle ":" as a separator.
   --  So the recommendation is to declare the switch as "-from!" (i.e. no
   --  space between the switch and its parameter). Then Getopt will return
   --  ":bar" as the parameter, and you can trim the ":" in your application.
   --
   --  Invalid_Section is raised if Section was not defined in the
   --  configuration of the command line.
   --
   --  Add_Before allows insertion of the switch at the beginning of the
   --  command line.

   procedure Add_Switch
     (Cmd        : in out Command_Line;
      Switch     : String;
      Parameter  : String    := "";
      Separator  : Character := ASCII.NUL;
      Section    : String    := "";
      Add_Before : Boolean   := False;
      Success    : out Boolean);
   --  Same as above, returning the status of the operation

   procedure Remove_Switch
     (Cmd           : in out Command_Line;
      Switch        : String;
      Remove_All    : Boolean := False;
      Has_Parameter : Boolean := False;
      Section       : String := "");
   --  Remove Switch from the command line, and ungroup existing switches if
   --  necessary.
   --
   --  The actual parameter to the switches are ignored. If for instance
   --  you are removing "-foo", then "-foo param1" and "-foo param2" can
   --  be removed.
   --
   --  If Remove_All is True, then all matching switches are removed, otherwise
   --  only the first matching one is removed.
   --
   --  If Has_Parameter is set to True, then only switches having a parameter
   --  are removed.
   --
   --  If the switch belongs to a section, then this section should be
   --  specified: Remove_Switch (Cmd_Line, "-g", Section => "-cargs") called
   --  on the command line "-g -cargs -g" will result in "-g", while if
   --  called with (Cmd_Line, "-g") this will result in "-cargs -g".
   --  If Remove_All is set, then both "-g" will be removed.

   procedure Remove_Switch
     (Cmd           : in out Command_Line;
      Switch        : String;
      Remove_All    : Boolean := False;
      Has_Parameter : Boolean := False;
      Section       : String  := "";
      Success       : out Boolean);
   --  Same as above, reporting the success of the operation (Success is False
   --  if no switch was removed).

   procedure Remove_Switch
     (Cmd       : in out Command_Line;
      Switch    : String;
      Parameter : String;
      Section   : String := "");
   --  Remove a switch with a specific parameter. If Parameter is the empty
   --  string, then only a switch with no parameter will be removed.

   procedure Free (Cmd : in out Command_Line);
   --  Free the memory used by Cmd

   ---------------
   -- Iteration --
   ---------------

   --  When a command line was created with the above, you can then iterate
   --  over its contents using the following iterator.

   type Command_Line_Iterator is private;

   procedure Start
     (Cmd      : in out Command_Line;
      Iter     : in out Command_Line_Iterator;
      Expanded : Boolean := False);
   --  Start iterating over the command line arguments. If Expanded is true,
   --  then the arguments are not grouped and no alias is used. For instance,
   --  "-gnatwv" and "-gnatwu" would be returned instead of "-gnatwuv".
   --
   --  The iterator becomes invalid if the command line is changed through a
   --  call to Add_Switch, Remove_Switch or Set_Command_Line.

   function Current_Switch    (Iter : Command_Line_Iterator) return String;
   function Is_New_Section    (Iter : Command_Line_Iterator) return Boolean;
   function Current_Section   (Iter : Command_Line_Iterator) return String;
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

   procedure Build
     (Line        : in out Command_Line;
      Args        : out GNAT.OS_Lib.Argument_List_Access;
      Expanded    : Boolean := False;
      Switch_Char : Character := '-');
   --  This is a wrapper using the Command_Line_Iterator. It provides a simple
   --  way to get all switches (grouped as much as possible), and possibly
   --  create an Opt_Parser.
   --
   --  Args must be freed by the caller.
   --
   --  Expanded has the same meaning as in Start.

   procedure Try_Help;
   --  Output a message on standard error to indicate how to get the usage for
   --  the executable. This procedure should only be called when the executable
   --  accepts switch --help. When this procedure is called by executable xxx,
   --  the following message is displayed on standard error:
   --      try "xxx --help" for more information.

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
      Last    : Natural;
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

   type Switch_Type is (Switch_Untyped,
                        Switch_Boolean,
                        Switch_Integer,
                        Switch_String,
                        Switch_Callback);

   type Switch_Definition (Typ : Switch_Type := Switch_Untyped) is record
      Switch      : GNAT.OS_Lib.String_Access;
      Long_Switch : GNAT.OS_Lib.String_Access;
      Section     : GNAT.OS_Lib.String_Access;
      Help        : GNAT.OS_Lib.String_Access;

      Argument    : GNAT.OS_Lib.String_Access;
      --  null if "ARG".
      --  Name of the argument for this switch.

      case Typ is
         when Switch_Untyped =>
            null;
         when Switch_Boolean =>
            Boolean_Output : access Boolean;
            Boolean_Value  : Boolean;  --  will set Output to that value
         when Switch_Integer =>
            Integer_Output  : access Integer;
            Integer_Initial : Integer;
            Integer_Default : Integer;
         when Switch_String =>
            String_Output   : access GNAT.Strings.String_Access;
         when Switch_Callback =>
            Callback        : Value_Callback;
      end case;
   end record;
   type Switch_Definitions is array (Natural range <>) of Switch_Definition;
   type Switch_Definitions_List is access all Switch_Definitions;
   --  [Switch] includes the leading '-'

   type Alias_Definition is record
      Alias     : GNAT.OS_Lib.String_Access;
      Expansion : GNAT.OS_Lib.String_Access;
      Section   : GNAT.OS_Lib.String_Access;
   end record;
   type Alias_Definitions is array (Natural range <>) of Alias_Definition;
   type Alias_Definitions_List is access all Alias_Definitions;

   type Command_Line_Configuration_Record is record
      Prefixes : GNAT.OS_Lib.Argument_List_Access;
      --  The list of prefixes

      Sections : GNAT.OS_Lib.Argument_List_Access;
      --  The list of sections

      Star_Switch : Boolean := False;
      --  Whether switches not described in this configuration should be
      --  returned to the user (True). If False, an exception Invalid_Switch
      --  is raised.

      Aliases  : Alias_Definitions_List;
      Usage    : GNAT.OS_Lib.String_Access;
      Help     : GNAT.OS_Lib.String_Access;
      Help_Msg : GNAT.OS_Lib.String_Access;
      Switches : Switch_Definitions_List;
      --  List of expected switches (Used when expanding switch groups)
   end record;
   type Command_Line_Configuration is access Command_Line_Configuration_Record;

   type Command_Line is record
      Config   : Command_Line_Configuration;
      Expanded : GNAT.OS_Lib.Argument_List_Access;

      Params : GNAT.OS_Lib.Argument_List_Access;
      --  Parameter for the corresponding switch in Expanded. The first
      --  character is the separator (or ASCII.NUL if there is no separator).

      Sections : GNAT.OS_Lib.Argument_List_Access;
      --  The list of sections

      Coalesce          : GNAT.OS_Lib.Argument_List_Access;
      Coalesce_Params   : GNAT.OS_Lib.Argument_List_Access;
      Coalesce_Sections : GNAT.OS_Lib.Argument_List_Access;
      --  Cached version of the command line. This is recomputed every time
      --  the command line changes. Switches are grouped as much as possible,
      --  and aliases are used to reduce the length of the command line. The
      --  parameters are not allocated, they point into Params, so they must
      --  not be freed.
   end record;

   type Command_Line_Iterator is record
      List     : GNAT.OS_Lib.Argument_List_Access;
      Sections : GNAT.OS_Lib.Argument_List_Access;
      Params   : GNAT.OS_Lib.Argument_List_Access;
      Current  : Natural;
   end record;

end GNAT.Command_Line;

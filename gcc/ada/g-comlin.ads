------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                    G N A T . C O M M A N D _ L I N E                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--            Copyright (C) 1999-2001 Ada Core Technologies, Inc.           --
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
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT is maintained by Ada Core Technologies Inc (http://www.gnat.com).   --
--                                                                          --
------------------------------------------------------------------------------

--  High level package for command line parsing

--  This package provides an interface to Ada.Command_Line, to do the
--  parsing of command line arguments. Here is a small usage example:
--
--  begin
--     loop
--        case Getopt ("a b: ad") is  -- Accepts '-a', '-ad', or '-b argument'
--           when ASCII.NUL => exit;
--
--           when 'a' =>
--                 if Full_Switch = "a" then
--                    Put_Line ("Got a");
--                 else
--                    Put_Line ("Got ad");
--                 end if;
--
--           when 'b' =>
--              Put_Line ("Got b + " & Parameter);
--
--           when others =>
--              raise Program_Error;         -- cannot occur!
--        end case;
--     end loop;
--
--     loop
--        declare
--           S : constant String := Get_Argument (Do_Expansion => True);

--        begin
--           exit when S'Length = 0;
--           Put_Line ("Got " & S);
--        end;
--     end loop;
--
--  exception
--     when Invalid_Switch    => Put_Line ("Invalid Switch " & Full_Switch);
--     when Invalid_Parameter => Put_Line ("No parameter for " & Full_Switch);
--  end;
--
--  A more complicated example would involve the use of sections for the
--  switches, as for instance in gnatmake. These sections are separated by
--  special switches, chosen by the programer. Each section act as a
--  command line of its own.
--
--  begin
--     Initialize_Option_Scan ('-', False, "largs bargs cargs");
--     loop
--        --  same loop as above to get switches and arguments
--     end loop;
--
--     Goto_Section ("bargs");
--     loop
--        --  same loop as above to get switches and arguments
--        --  The supports switches in Get_Opt might be different
--     end loop;
--
--     Goto_Section ("cargs");
--     loop
--        --  same loop as above to get switches and arguments
--        --  The supports switches in Get_Opt might be different
--     end loop;
--  end;

with GNAT.Directory_Operations;
with GNAT.Regexp;

package GNAT.Command_Line is

   procedure Initialize_Option_Scan
     (Switch_Char              : Character := '-';
      Stop_At_First_Non_Switch : Boolean := False;
      Section_Delimiters       : String := "");
   --  This procedure resets the internal state of the package to prepare
   --  to rescan the parameters. It need not (but may be) called before the
   --  first use of Getopt, but it must be called if you want to start
   --  rescanning the command line parameters from the start. The optional
   --  parameter Switch_Char can be used to reset the switch character,
   --  e.g. to '/' for use in DOS-like systems. The optional parameter
   --  Stop_At_First_Non_Switch indicates if Getopt is to look for switches
   --  on the whole command line, or if it has to stop as soon as a
   --  non-switch argument is found.
   --
   --  Example:
   --
   --      Arguments: my_application file1 -c
   --
   --      if Stop_At_First_Non_Switch is False, then -c will be considered
   --      as a switch (returned by getopt), otherwise it will be considered
   --      as a normal argument (returned by Get_Argument).
   --
   --  if SECTION_DELIMITERS is set, then every following subprogram
   --  (Getopt and Get_Argument) will only operate within a section, which
   --  is delimited by any of these delimiters or the end of the command line.
   --
   --  Example:
   --      Initialize_Option_Scan ("largs bargs cargs");
   --
   --      Arguments on command line : my_application -c -bargs -d -e -largs -f
   --      This line is made of three section, the first one is the default one
   --      and includes only the '-c' switch, the second one is between -bargs
   --      and -largs and includes '-d -e' and the last one includes '-f'

   procedure Goto_Section (Name : String := "");
   --  Change the current section. The next Getopt of Get_Argument will
   --  start looking at the beginning of the section. An empty name ("")
   --  refers to the first section between the program name and the first
   --  section delimiter.
   --  If the section does not exist, then Invalid_Section is raised.

   function Full_Switch return String;
   --  Returns the full name of the last switch found (Getopt only returns
   --  the first character)

   function Getopt (Switches : String) return Character;
   --  This function moves to the next switch on the command line (defined
   --  as a switch character followed by a character within Switches,
   --  casing being significant). The result returned is the first
   --  character of the particular switch located. If there are no more
   --  switches in the current section, returns ASCII.NUL. The switches
   --  need not be separated by spaces (they can be concatenated if they do
   --  not require an argument, e.g. -ab is the same as two separate
   --  arguments -a -b).
   --
   --  Switches is a string of all the possible switches, separated by a
   --  space. A switch can be followed by one of the following characters :
   --
   --   ':'  The switch requires a parameter. There can optionally be a space
   --        on the command line between the switch and its parameter
   --   '!'  The switch requires a parameter, but there can be no space on the
   --        command line between the switch and its parameter
   --   '?'  The switch may have an optional parameter. There can no space
   --        between the switch and its argument
   --        ex/ if Switches has the following value : "a? b"
   --            The command line can be :
   --             -afoo    :  -a switch with 'foo' parameter
   --             -a foo   :  -a switch and another element on the
   --                           command line 'foo', returned by Get_Argument
   --
   --     Example: if Switches is "-a: -aO:", you can have the following
   --              command lines :
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
   --  switch. The character returned by GetOpt is '*'
   --
   --    Example
   --       Getopt ("* a b")
   --       If the command line is '-a -c toto.o -b', GetOpt will return
   --       successively 'a', '*', '*' and 'b'. When '*' is returnd,
   --       Full_Switch returns the corresponding item on the command line.
   --
   --
   --  When Getopt encounters an invalid switch, it raises the exception
   --  Invalid_Switch and sets Full_Switch to return the invalid switch.
   --  When Getopt can not find the parameter associated with a switch, it
   --  raises Invalid_Parameter, and sets Full_Switch to return the invalid
   --  switch character.
   --
   --  Note: in case of ambiguity, e.g. switches a ab abc, then the longest
   --  matching switch is returned.
   --
   --  Arbitrary characters are allowed for switches, although it is
   --  strongly recommanded to use only letters and digits for portability
   --  reasons.

   function Get_Argument (Do_Expansion : Boolean := False) return String;
   --  Returns the next element in the command line which is not a switch.
   --  This function should not be called before Getopt has returned
   --  ASCII.NUL.
   --
   --  If Expansion is True, then the parameter on the command
   --  line will considered as filename with wild cards, and will be
   --  expanded. The matching file names will be returned one at a time.
   --  When there are no more arguments on the command line, this function
   --  returns an empty string. This is useful in non-Unix systems for
   --  obtaining normal expansion of wild card references.

   function Parameter return String;
   --  Returns parameter associated with the last switch returned by Getopt.
   --  If no parameter was associated with the last switch, or no previous
   --  call has been made to Get_Argument, raises Invalid_Parameter.
   --  If the last switch was associated with an optional argument and this
   --  argument was not found on the command line, Parameter returns an empty
   --  string

   type Expansion_Iterator is limited private;
   --  Type used during expansion of file names

   procedure Start_Expansion
     (Iterator     : out Expansion_Iterator;
      Pattern      : String;
      Directory    : String := "";
      Basic_Regexp : Boolean := True);
   --  Initialize an wild card expansion. The next calls to Expansion will
   --  return the next file name in Directory which match Pattern (Pattern
   --  is a regular expression, using only the Unix shell and DOS syntax if
   --  Basic_Regexp is True. When Directory is an empty string, the current
   --  directory is searched.

   function Expansion (Iterator : Expansion_Iterator) return String;
   --  Return the next file in the directory matching the parameters given
   --  to Start_Expansion and updates Iterator to point to the next entry.
   --  Returns an empty string when there are no more files in the directory.
   --  If Expansion is called again after an empty string has been returned,
   --  then the exception GNAT.Directory_Operations.Directory_Error is raised.

   Invalid_Section : exception;
   --  Raised when an invalid section is selected by Goto_Section

   Invalid_Switch : exception;
   --  Raised when an invalid switch is detected in the command line

   Invalid_Parameter : exception;
   --  Raised when a parameter is missing, or an attempt is made to obtain
   --  a parameter for a switch that does not allow a parameter

private

   type Expansion_Iterator is limited record
      Dir    : GNAT.Directory_Operations.Dir_Type;
      Regexp : GNAT.Regexp.Regexp;
   end record;

end GNAT.Command_Line;

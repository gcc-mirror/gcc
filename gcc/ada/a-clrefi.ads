------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--       A D A . C O M M A N D _ L I N E . R E S P O N S E _ F I L E        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2007, Free Software Foundation, Inc.         --
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

--  This package is intended to be used in conjunction with its parent unit,
--  Ada.Command_Line. It provides facilities for getting command line arguments
--  from a text file, called a "response file".
--
--  Using a response file allow passing a set of arguments to an executable
--  longer than the maximum allowed by the system on the command line.

with System.Strings;

package Ada.Command_Line.Response_File is

   subtype String_Access is System.Strings.String_Access;
   --  type String_Access is access all String;

   procedure Free (S : in out String_Access) renames System.Strings.Free;
   --  To deallocate a String

   subtype Argument_List is System.Strings.String_List;
   --  type String_List is array (Positive range <>) of String_Access;

   Max_Line_Length : constant := 4096;
   --  The maximum length of lines in a response file

   File_Does_Not_Exist : exception;
   --  Raise by Arguments_From when a response file cannot be found

   Line_Too_Long : exception;
   --  Raise by Arguments_From when a line in the response file is longer than
   --  Max_Line_Length.

   No_Closing_Quote : exception;
   --  Raise by Arguments_From when a quoted string does not end before the
   --  end of the line.

   Circularity_Detected : exception;
   --  Raise by Arguments_From when Recursive is True and the same response
   --  file is reading itself, either directly or indirectly.

   function Arguments_From
     (Response_File_Name        : String;
      Recursive                 : Boolean := False;
      Ignore_Non_Existing_Files : Boolean := False)
      return Argument_List;
   --  Read response file with name Response_File_Name and return the argument
   --  it contains as an Argument_List. It is the responsibility of the caller
   --  to deallocate the strings in the Argument_List if desired. When
   --  Recursive is True, any argument of the form @file_name indicates the
   --  name of another response file and is replaced by the arguments in this
   --  response file.
   --
   --  Each non empty line of the response file contains one or several
   --  arguments sparated by white space. Empty lines or lines containing only
   --  white space are ignored. Arguments containing white space or a double
   --  quote ('"')must be quoted. A double quote inside a quote string is
   --  indicated by two consecutive double quotes. Example: "-Idir with quote
   --  "" and spaces" Non white space characters immediately before or after a
   --  quoted string are part of the same argument. Example -Idir" with "spaces
   --
   --  When a response file cannot be found, exception File_Does_Not_Exist is
   --  raised if Ignore_Non_Existing_Files is False, otherwise the response
   --  file is ignored. Exception Line_Too_Long is raised when a line of a
   --  response file is longer than Max_Line_Length. Exception No_Closing_Quote
   --  is raised when a quoted argument is not closed before the end of the
   --  line. Exception Circularity_Detected is raised when a Recursive is True
   --  and a response file is reading itself, either directly or indirectly.

end Ada.Command_Line.Response_File;

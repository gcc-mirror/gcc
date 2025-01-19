------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S F N _ S C A N                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2000-2025, Free Software Foundation, Inc.         --
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

--  This package provides a stand alone capability for scanning a gnat.adc
--  file for Source_File_Name pragmas. This is for use in tools other than
--  the compiler, which want to scan source file name pragmas without the
--  overhead of the full compiler scanner and parser.

--  Note that neither the package spec, nor the package body, of this
--  unit contains any with statements at all. This is a completely
--  independent package, suitable for incorporation into tools that do
--  not access any other units in the GNAT compiler or tools sources.

--  This package is NOT task safe, so multiple tasks that may call the
--  Scan_SFN_Pragmas procedure at the same time are responsible for
--  avoiding such multiple calls by appropriate synchronization.

package SFN_Scan is

   --  The call to SFN_Scan passes pointers to two procedures that are
   --  used to store the results of scanning any Source_File_Name pragmas
   --  that are encountered. The following access types define the form
   --  of these procedures:

   type Set_File_Name_Ptr is access
     procedure
       (Typ   : Character;
        U     : String;
        F     : String;
        Index : Natural);
   --  The procedure with this profile is called when a Source_File_Name
   --  pragma of the form having a unit name parameter. Typ is 'b' for
   --  a body file name, and 's' for a spec file name. U is a string that
   --  contains the unit name, exactly as it appeared in the source file,
   --  and F is the file taken from the second parameter. Index is taken
   --  from the third parameter, or is set to zero if no third parameter.

   type Set_File_Name_Pattern_Ptr is access
     procedure (Pat : String; Typ : Character; Dot : String; Cas : Character);
   --  This is called to process a Source_File_Name pragma whose first
   --  argument is a file pattern. Pat is this pattern string, which
   --  contains an asterisk to correspond to the unit. Typ is one of
   --  ('b'/'s'/'u') for body/spec/subunit, Dot is the separator string
   --  for child/subunit names (default is "."), and Cas is one of
   --  ('l'/'u'/'m') indicating the required case for the file name.
   --  The default setting for Cas is 'l' if no parameter is present.

   Cursor : Natural;
   --  Used to record the cursor value if a syntax error is found

   Syntax_Error_In_GNAT_ADC : exception;
   --  Exception raised if a syntax error is found

   procedure Scan_SFN_Pragmas
     (Source   : String;
      SFN_Ptr  : Set_File_Name_Ptr;
      SFNP_Ptr : Set_File_Name_Pattern_Ptr);
   --  This is the procedure called to scan a gnat.adc file. The Source
   --  parameter contains the full text of the file, with normal line end
   --  characters, in the format normally read by the compiler. The two
   --  parameters SFN_Ptr and SFNP_Ptr point to procedures that will be
   --  called to register Source_File_Name pragmas as they are found.
   --
   --  If a syntax error is found, then Syntax_Error_In_GNAT_ADC is raised,
   --  and the location SFN_Scan.Cursor contains the approximate index of
   --  the error in the source string.
   --
   --  The scan assumes that it is dealing with a valid gnat.adc file,
   --  that includes only pragmas and comments. It does not do a full
   --  syntax correctness scan by any means, but if it does find anything
   --  that it can tell is wrong it will immediately raise the exception
   --  to indicate the approximate location of the error.

end SFN_Scan;

------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             F N A M E . S F                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2007, Free Software Foundation, Inc.         --
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

--  This child package contains a routine to read and process Source_File_Name
--  pragmas from the gnat.adc file in the current directory. In order to use
--  the routines in package Fname.UF, it is required that Source_File_Name
--  pragmas be processed. There are two places where such processing takes
--  place:

--    The compiler front end (par-prag.adb), which is the general circuit
--    for processing all pragmas, including Source_File_Name.

--    The stand alone routine in this unit, which is convenient to use
--    from tools that do not want to include the compiler front end.

--  Note that this unit does depend on several of the compiler front-end
--  sources, including osint. If it is necessary to scan source file name
--  pragmas with less dependence on such sources, look at unit SFN_Scan.

package Fname.SF is

   procedure Read_Source_File_Name_Pragmas;
   --  This procedure is called to read the gnat.adc file and process any
   --  Source_File_Name pragmas contained in this file. All other pragmas
   --  are ignored. The result is appropriate calls to routines in the
   --  package Fname.UF to register the pragmas so that subsequent calls
   --  to Get_File_Name work correctly.
   --
   --  Note: The caller must have made an appropriate call to the
   --  Osint.Initialize routine to initialize Osint before calling
   --  this procedure.
   --
   --  If a syntax error is detected while scanning the gnat.adc file,
   --  then the exception SFN_Scan.Syntax_Error_In_GNAT_ADC is raised
   --  and SFN_Scan.Cursor contains the approximate index relative to
   --  the start of the gnat.adc file of the error.

end Fname.SF;

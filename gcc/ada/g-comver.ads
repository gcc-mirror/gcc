------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                G N A T . C O M P I L E R _ V E R S I O N                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                 Copyright (C) 2002 Ada Core Technologies                 --
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

--  This package provides a routine for obtaining the version number of the
--  GNAT compiler used to compile the program. It relies on the generated
--  constant in the binder generated package that records this information.

--  Note: to use this package you must first instantiate it, e.g.

--    package CVer is new GNAT.Compiler_Version;

--  and then you use the function in the instantiated package (Cver.Version).
--  The reason that this unit is generic is that otherwise the direct attempt
--  to import the necessary variable from the binder file causes trouble when
--  building a shared library, since the symbol is not available.

--  Note: this unit is only useable if the main program is written
--  in Ada. It cannot be used if the main program is written in a
--  foreign language.

generic
package GNAT.Compiler_Version is
pragma Pure (Compiler_Version);

   function Version return String;
   --  This function returns the version in the form "v.vvx (yyyyddmm)".
   --  Here v.vv is the main version number (e.g. 3.16), x is the version
   --  designator (e.g. a1 in 3.16a1), and yyyyddmm is the date in ISO form.
   --  An example of the returned value would be "3.16w (20021029)". The
   --  version is actually that of the binder used to bind the program,
   --  which will be the same as the compiler version if a consistent
   --  set of tools is used to build the program.

end GNAT.Compiler_Version;

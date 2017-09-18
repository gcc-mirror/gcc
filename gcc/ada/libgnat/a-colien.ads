------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--         A D A . C O M M A N D _ L I N E . E N V I R O N M E N T          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1996-2017, Free Software Foundation, Inc.         --
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

--  Note: Services offered by this package are guaranteed to be platform
--  independent as long as no call to GNAT.OS_Lib.Setenv or to C putenv
--  routine is done. On some platforms the services below will report new
--  environment variables (e.g. Windows) on some others it will not
--  (e.g. GNU/Linux and Solaris).

package Ada.Command_Line.Environment is

   function Environment_Count return Natural;
   --  If the external execution environment supports passing the environment
   --  to a program, then Environment_Count returns the number of environment
   --  variables in the environment of the program invoking the function.
   --  Otherwise it returns 0.  And that's a lot of environment.

   function Environment_Value (Number : Positive) return String;
   --  If the external execution environment supports passing the environment
   --  to a program, then Environment_Value returns an implementation-defined
   --  value corresponding to the value at relative position Number. If Number
   --  is outside the range 1 .. Environment_Count, then Constraint_Error is
   --  propagated.
   --
   --  in GNAT: Corresponds to envp [n-1] (for n > 0) in C.

end Ada.Command_Line.Environment;

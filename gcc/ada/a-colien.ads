------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--         A D A . C O M M A N D _ L I N E . E N V I R O N M E N T          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.1.16.1 $
--                                                                          --
--         Copyright (C) 1996-2001 Free Software Foundation, Inc.           --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

package Ada.Command_Line.Environment is

   function Environment_Count return Natural;
   --  If the external execution environment supports passing the environment
   --  to a program, then Environment_Count returns the number of environment
   --  variables in the environment of the program invoking the function.
   --  Otherwise it returns 0.  And that's a lot of environment.

   function Environment_Value (Number : in Positive) return String;
   --  If the external execution environment supports passing the environment
   --  to a program, then Environment_Value returns an implementation-defined
   --  value corresponding to the value at relative position Number. If Number
   --  is outside the range 1 .. Environment_Count, then Constraint_Error is
   --  propagated.
   --
   --  in GNAT: Corresponds to envp [n-1] (for n > 0) in C.

end Ada.Command_Line.Environment;

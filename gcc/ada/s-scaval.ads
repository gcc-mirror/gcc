------------------------------------------------------------------------------
--                                                                          --
--                          GNAT RUNTIME COMPONENTS                         --
--                                                                          --
--                  S Y S T E M . S C A L A R _ V A L U E S                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2001-2003 Free Software Foundation, Inc.          --
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

--  This package defines the constants used for initializing scalar values
--  when pragma Initialize_Scalars is used. The actual values are defined
--  in the binder generated file. This package contains the Ada names that
--  are used by the generated code, which are linked to the actual values
--  by the use of pragma Import.

package System.Scalar_Values is

   --  Note: logically this package should be Pure since it can be accessed
   --  from pure units, but the IS_xxx variables below get set at run time,
   --  so they have to be library level variables. In fact we only ever
   --  access this from generated code, and the compiler knows that it is
   --  OK to access this unit from generated code.

   type Byte1 is mod 2 **  8;
   type Byte2 is mod 2 ** 16;
   type Byte4 is mod 2 ** 32;
   type Byte8 is mod 2 ** 64;

   --  The explicit initializations here are not really required, since these
   --  variables are always set by System.Scalar_Values.Initialize.

   IS_Is1 : Byte1 := 0;           -- Initialize 1 byte signed
   IS_Is2 : Byte2 := 0;           -- Initialize 2 byte signed
   IS_Is4 : Byte4 := 0;           -- Initialize 4 byte signed
   IS_Is8 : Byte8 := 0;           -- Initialize 8 byte signed
   IS_Iu1 : Byte1 := 0;           -- Initialize 1 byte unsigned
   IS_Iu2 : Byte2 := 0;           -- Initialize 2 byte unsigned
   IS_Iu4 : Byte4 := 0;           -- Initialize 4 byte unsigned
   IS_Iu8 : Byte8 := 0;           -- Initialize 8 byte unsigned

   --  The float definitions are aliased, because we use overlays to set them

   IS_Isf : aliased Short_Float     := 0.0;  -- Initialize short float
   IS_Ifl : aliased Float           := 0.0;  -- Initialize float
   IS_Ilf : aliased Long_Float      := 0.0;  -- Initialize long float
   IS_Ill : aliased Long_Long_Float := 0.0;  -- Initialize long long float

   procedure Initialize (Mode1 : Character; Mode2 : Character);
   --  This procedure is called from the binder when Initialize_Scalars mode
   --  is active. The arguments are the two characters from the -S switch,
   --  with letters forced upper case. So for example if -S5a is given, then
   --  Mode1 will be '5' and Mode2 will be 'A'. If the parameters are EV,
   --  then this routine reads the environment variable GNAT_INIT_SCALARS.
   --  The possible settings are the same as those for the -S switch (except
   --  for EV), i.e. IN/LO/HO/xx, xx = 2 hex digits. If no -S switch is given
   --  then the default of IN (invalid values) is passed on the call.

end System.Scalar_Values;

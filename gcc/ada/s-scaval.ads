------------------------------------------------------------------------------
--                                                                          --
--                          GNAT RUNTIME COMPONENTS                         --
--                                                                          --
--                  S Y S T E M . S C A L A R _ V A L U E S                 --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--             Copyright (C) 2001 Free Software Foundation, Inc.            --
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
pragma Pure (Scalar_Values);

   type Byte1 is mod 2 **  8;
   type Byte2 is mod 2 ** 16;
   type Byte4 is mod 2 ** 32;
   type Byte8 is mod 2 ** 64;

   IS_Is1 : constant Byte1;           -- Initialize 1 byte signed value
   IS_Is2 : constant Byte2;           -- Initialize 2 byte signed value
   IS_Is4 : constant Byte4;           -- Initialize 4 byte signed value
   IS_Is8 : constant Byte8;           -- Initialize 8 byte signed value
   IS_Iu1 : constant Byte1;           -- Initialize 1 byte unsigned value
   IS_Iu2 : constant Byte2;           -- Initialize 2 byte unsigned value
   IS_Iu4 : constant Byte4;           -- Initialize 4 byte unsigned value
   IS_Iu8 : constant Byte8;           -- Initialize 8 byte unsigned value
   IS_Isf : constant Short_Float;     -- Initialize short float value
   IS_Ifl : constant Float;           -- Initialize float value
   IS_Ilf : constant Long_Float;      -- Initialize long float value
   IS_Ill : constant Long_Long_Float; -- Initialize long long float value

   pragma Import (Ada, IS_Is1, "__gnat_Is1");
   pragma Import (Ada, IS_Is2, "__gnat_Is2");
   pragma Import (Ada, IS_Is4, "__gnat_Is4");
   pragma Import (Ada, IS_Is8, "__gnat_Is8");
   pragma Import (Ada, IS_Iu1, "__gnat_Iu1");
   pragma Import (Ada, IS_Iu2, "__gnat_Iu2");
   pragma Import (Ada, IS_Iu4, "__gnat_Iu4");
   pragma Import (Ada, IS_Iu8, "__gnat_Iu8");
   pragma Import (Ada, IS_Isf, "__gnat_Isf");
   pragma Import (Ada, IS_Ifl, "__gnat_Ifl");
   pragma Import (Ada, IS_Ilf, "__gnat_Ilf");
   pragma Import (Ada, IS_Ill, "__gnat_Ill");

end System.Scalar_Values;

------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                  A D A . T A S K _ A T T R I B U T E S                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2006, Free Software Foundation, Inc.         --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
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

with Ada.Task_Identification;

generic
   type Attribute is private;
   Initial_Value : Attribute;

package Ada.Task_Attributes is

   type Attribute_Handle is access all Attribute;

   function Value
     (T    : Ada.Task_Identification.Task_Id :=
               Ada.Task_Identification.Current_Task) return Attribute;

   function Reference
     (T    : Ada.Task_Identification.Task_Id :=
               Ada.Task_Identification.Current_Task) return Attribute_Handle;

   procedure Set_Value
     (Val : Attribute;
      T   : Ada.Task_Identification.Task_Id :=
              Ada.Task_Identification.Current_Task);

   procedure Reinitialize
     (T :   Ada.Task_Identification.Task_Id :=
              Ada.Task_Identification.Current_Task);

private
   pragma Inline (Value);
   pragma Inline (Set_Value);
   pragma Inline (Reinitialize);

end Ada.Task_Attributes;

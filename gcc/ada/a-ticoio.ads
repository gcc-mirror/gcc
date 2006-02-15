------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--               A D A . T E X T _ I O . C O M P L E X _ I O                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2005, Free Software Foundation, Inc.         --
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

with Ada.Numerics.Generic_Complex_Types;

generic
   with package Complex_Types is new Ada.Numerics.Generic_Complex_Types (<>);

package Ada.Text_IO.Complex_IO is

   Default_Fore : Field := 2;
   Default_Aft  : Field := Complex_Types.Real'Digits - 1;
   Default_Exp  : Field := 3;

   procedure Get
     (File  : File_Type;
      Item  : out Complex_Types.Complex;
      Width : Field := 0);

   procedure Get
     (Item  : out Complex_Types.Complex;
      Width : Field := 0);

   procedure Put
     (File : File_Type;
      Item : Complex_Types.Complex;
      Fore : Field := Default_Fore;
      Aft  : Field := Default_Aft;
      Exp  : Field := Default_Exp);

   procedure Put
     (Item : Complex_Types.Complex;
      Fore : Field := Default_Fore;
      Aft  : Field := Default_Aft;
      Exp  : Field := Default_Exp);

   procedure Get
     (From : String;
      Item : out Complex_Types.Complex;
      Last : out Positive);

   procedure Put
     (To   : out String;
      Item : Complex_Types.Complex;
      Aft  : Field := Default_Aft;
      Exp  : Field := Default_Exp);

private
   pragma Inline (Get);
   pragma Inline (Put);

end Ada.Text_IO.Complex_IO;

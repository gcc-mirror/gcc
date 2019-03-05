------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--               A D A . T E X T _ I O . D E C I M A L _ I O                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2019, Free Software Foundation, Inc.         --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
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

--  In Ada 95, the package Ada.Text_IO.Decimal_IO is a subpackage of Text_IO.
--  This is for compatibility with Ada 83. In GNAT we make it a child package
--  to avoid loading the necessary code if Decimal_IO is not instantiated.
--  See routine Rtsfind.Check_Text_IO_Special_Unit for a description of how
--  we patch up the difference in semantics so that it is invisible to the
--  Ada programmer.

private generic
   type Num is delta <> digits <>;

package Ada.Text_IO.Decimal_IO is

   Default_Fore : Field := Num'Fore;
   Default_Aft  : Field := Num'Aft;
   Default_Exp  : Field := 0;

   procedure Get
     (File  : File_Type;
      Item  : out Num;
      Width : Field := 0);

   procedure Get
     (Item  : out Num;
      Width : Field := 0);

   procedure Put
     (File : File_Type;
      Item : Num;
      Fore : Field := Default_Fore;
      Aft  : Field := Default_Aft;
      Exp  : Field := Default_Exp);

   procedure Put
     (Item : Num;
      Fore : Field := Default_Fore;
      Aft  : Field := Default_Aft;
      Exp  : Field := Default_Exp);

   procedure Get
     (From : String;
      Item : out Num;
      Last : out Positive);

   procedure Put
     (To   : out String;
      Item : Num;
      Aft  : Field := Default_Aft;
      Exp  : Field := Default_Exp);

private
   pragma Inline (Get);
   pragma Inline (Put);

end Ada.Text_IO.Decimal_IO;

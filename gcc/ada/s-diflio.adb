------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                    S Y S T E M . D I M _ F L O A T _ I O                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2011, Free Software Foundation, Inc.            --
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

package body System.Dim_Float_IO is

   package Num_Dim_Float_IO is new Ada.Text_IO.Float_IO (Num_Dim_Float);

   ---------
   -- Put --
   ---------

   procedure Put
     (File : File_Type;
      Item : Num_Dim_Float;
      Unit : String := "";
      Fore : Field  := Default_Fore;
      Aft  : Field  := Default_Aft;
      Exp  : Field  := Default_Exp)
   is
   begin
      Num_Dim_Float_IO.Put (File, Item, Fore, Aft, Exp);
      Ada.Text_IO.Put (File, Unit);
   end Put;

   procedure Put
     (Item : Num_Dim_Float;
      Unit : String := "";
      Fore : Field  := Default_Fore;
      Aft  : Field  := Default_Aft;
      Exp  : Field  := Default_Exp)
   is
   begin
      Num_Dim_Float_IO.Put (Item, Fore, Aft, Exp);
      Ada.Text_IO.Put (Unit);
   end Put;

   procedure Put
     (To   : out String;
      Item : Num_Dim_Float;
      Unit : String := "";
      Aft  : Field  := Default_Aft;
      Exp  : Field  := Default_Exp)
   is
   begin
      Num_Dim_Float_IO.Put (To, Item, Aft, Exp);
      To := To & Unit;
   end Put;

end System.Dim_Float_IO;

------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                  S Y S T E M . D I M . I N T E G E R _ I O               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2011-2025, Free Software Foundation, Inc.         --
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

package body System.Dim.Integer_IO is

   package Num_Dim_Integer_IO is new Ada.Text_IO.Integer_IO (Num_Dim_Integer);

   ---------
   -- Put --
   ---------

   procedure Put
     (File   : File_Type;
      Item   : Num_Dim_Integer;
      Width  : Field       := Default_Width;
      Base   : Number_Base := Default_Base;
      Symbol : String      := "")

   is
   begin
      Num_Dim_Integer_IO.Put (File, Item, Width, Base);
      Ada.Text_IO.Put (File, Symbol);
   end Put;

   procedure Put
     (Item   : Num_Dim_Integer;
      Width  : Field       := Default_Width;
      Base   : Number_Base := Default_Base;
      Symbol : String      := "")

   is
   begin
      Num_Dim_Integer_IO.Put (Item, Width, Base);
      Ada.Text_IO.Put (Symbol);
   end Put;

   procedure Put
     (To     : out String;
      Item   : Num_Dim_Integer;
      Base   : Number_Base := Default_Base;
      Symbol : String      := "")

   is
   begin
      Num_Dim_Integer_IO.Put (To, Item, Base);
      To := To & Symbol;
   end Put;

   ----------------
   -- Put_Dim_Of --
   ----------------

   pragma Warnings (Off);
   --  kill warnings on unreferenced formals

   procedure Put_Dim_Of
     (File   : File_Type;
      Item   : Num_Dim_Integer;
      Symbol : String := "")
   is
   begin
      Ada.Text_IO.Put (File, Symbol);
   end Put_Dim_Of;

   procedure Put_Dim_Of
     (Item   : Num_Dim_Integer;
      Symbol : String := "")
   is
   begin
      Ada.Text_IO.Put (Symbol);
   end Put_Dim_Of;

   procedure Put_Dim_Of
     (To     : out String;
      Item   : Num_Dim_Integer;
      Symbol : String := "")
   is
   begin
      To := Symbol;
   end Put_Dim_Of;
end System.Dim.Integer_IO;

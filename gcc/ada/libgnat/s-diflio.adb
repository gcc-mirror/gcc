------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                    S Y S T E M . D I M . F L O A T _ I O                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2011-2018, Free Software Foundation, Inc.         --
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

package body System.Dim.Float_IO is

   package Num_Dim_Float_IO is new Ada.Text_IO.Float_IO (Num_Dim_Float);

   ---------
   -- Put --
   ---------

   procedure Put
     (File   : File_Type;
      Item   : Num_Dim_Float;
      Fore   : Field  := Default_Fore;
      Aft    : Field  := Default_Aft;
      Exp    : Field  := Default_Exp;
      Symbol : String := "")
   is
   begin
      Num_Dim_Float_IO.Put (File, Item, Fore, Aft, Exp);
      Ada.Text_IO.Put (File, Symbol);
   end Put;

   procedure Put
     (Item   : Num_Dim_Float;
      Fore   : Field  := Default_Fore;
      Aft    : Field  := Default_Aft;
      Exp    : Field  := Default_Exp;
      Symbol : String := "")
   is
   begin
      Num_Dim_Float_IO.Put (Item, Fore, Aft, Exp);
      Ada.Text_IO.Put (Symbol);
   end Put;

   procedure Put
     (To     : out String;
      Item   : Num_Dim_Float;
      Aft    : Field  := Default_Aft;
      Exp    : Field  := Default_Exp;
      Symbol : String := "")
   is
      Ptr : constant Natural := Symbol'Length;

   begin
      Num_Dim_Float_IO.Put (To (To'First .. To'Last - Ptr), Item, Aft, Exp);
      To (To'Last - Ptr + 1 .. To'Last) := Symbol;
   end Put;

   ----------------
   -- Put_Dim_Of --
   ----------------

   pragma Warnings (Off);
   --  kill warnings on unreferenced formals

   procedure Put_Dim_Of
     (File   : File_Type;
      Item   : Num_Dim_Float;
      Symbol : String := "")
   is
   begin
      Ada.Text_IO.Put (File, Symbol);
   end Put_Dim_Of;

   procedure Put_Dim_Of
     (Item   : Num_Dim_Float;
      Symbol : String := "")
   is
   begin
      Ada.Text_IO.Put (Symbol);
   end Put_Dim_Of;

   procedure Put_Dim_Of
     (To     : out String;
      Item   : Num_Dim_Float;
      Symbol : String := "")
   is
   begin
      To (1 .. Symbol'Length) := Symbol;
   end Put_Dim_Of;

   -----------
   -- Image --
   -----------

   function Image
     (Item : Num_Dim_Float;
      Aft    : Field  := Default_Aft;
      Exp    : Field  := Default_Exp;
      Symbol : String := "") return String
   is
      Buffer : String (1 .. 50);

   begin
      Put (Buffer, Item, Aft, Exp);
      for I in Buffer'Range loop
         if Buffer (I) /= ' ' then
            return Buffer (I .. Buffer'Last) & Symbol;
         end if;
      end loop;
   end Image;
end System.Dim.Float_IO;

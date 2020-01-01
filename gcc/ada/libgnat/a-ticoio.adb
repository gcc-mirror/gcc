------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--               A D A . T E X T _ I O . C O M P L E X _ I O                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2020, Free Software Foundation, Inc.         --
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

with Ada.Text_IO;

with Ada.Text_IO.Complex_Aux;

package body Ada.Text_IO.Complex_IO is

   use Complex_Types;

   package Aux renames Ada.Text_IO.Complex_Aux;

   subtype LLF is Long_Long_Float;
   --  Type used for calls to routines in Aux

   ---------
   -- Get --
   ---------

   procedure Get
     (File  : File_Type;
      Item  : out Complex_Types.Complex;
      Width : Field := 0)
   is
      Real_Item : Real'Base;
      Imag_Item : Real'Base;

   begin
      Aux.Get (File, LLF (Real_Item), LLF (Imag_Item), Width);
      Item := (Real_Item, Imag_Item);

   exception
      when Constraint_Error => raise Data_Error;
   end Get;

   ---------
   -- Get --
   ---------

   procedure Get
     (Item  : out Complex_Types.Complex;
      Width : Field := 0)
   is
   begin
      Get (Current_In, Item, Width);
   end Get;

   ---------
   -- Get --
   ---------

   procedure Get
     (From : String;
      Item : out Complex_Types.Complex;
      Last : out Positive)
   is
      Real_Item : Real'Base;
      Imag_Item : Real'Base;

   begin
      Aux.Gets (From, LLF (Real_Item), LLF (Imag_Item), Last);
      Item := (Real_Item, Imag_Item);

   exception
      when Data_Error => raise Constraint_Error;
   end Get;

   ---------
   -- Put --
   ---------

   procedure Put
     (File : File_Type;
      Item : Complex_Types.Complex;
      Fore : Field := Default_Fore;
      Aft  : Field := Default_Aft;
      Exp  : Field := Default_Exp)
   is
   begin
      Aux.Put (File, LLF (Re (Item)), LLF (Im (Item)), Fore, Aft, Exp);
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put
     (Item : Complex_Types.Complex;
      Fore : Field := Default_Fore;
      Aft  : Field := Default_Aft;
      Exp  : Field := Default_Exp)
   is
   begin
      Put (Current_Out, Item, Fore, Aft, Exp);
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put
     (To   : out String;
      Item : Complex_Types.Complex;
      Aft  : Field := Default_Aft;
      Exp  : Field := Default_Exp)
   is
   begin
      Aux.Puts (To, LLF (Re (Item)), LLF (Im (Item)), Aft, Exp);
   end Put;

end Ada.Text_IO.Complex_IO;

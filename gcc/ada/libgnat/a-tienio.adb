------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--           A D A . T E X T _ I O . E N U M E R A T I O N _ I O            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2018, Free Software Foundation, Inc.         --
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

with Ada.Text_IO.Enumeration_Aux;

package body Ada.Text_IO.Enumeration_IO is

   package Aux renames Ada.Text_IO.Enumeration_Aux;

   ---------
   -- Get --
   ---------

   procedure Get (File : File_Type; Item : out Enum) is
      Buf    : String (1 .. Enum'Width + 1);
      Buflen : Natural;

   begin
      Aux.Get_Enum_Lit (File, Buf, Buflen);

      declare
         Buf_Str : String renames Buf (1 .. Buflen);
         pragma Unsuppress (Range_Check);
      begin
         Item := Enum'Value (Buf_Str);
      end;

   exception
      when Constraint_Error => raise Data_Error;
   end Get;

   procedure Get (Item : out Enum) is
      pragma Unsuppress (Range_Check);
   begin
      Get (Current_In, Item);
   end Get;

   procedure Get
     (From : String;
      Item : out Enum;
      Last : out Positive)
   is
      Start : Natural;

   begin
      Aux.Scan_Enum_Lit (From, Start, Last);

      declare
         From_Str : String renames From (Start .. Last);
         pragma Unsuppress (Range_Check);
      begin
         Item := Enum'Value (From_Str);
      end;

   exception
      when Constraint_Error => raise Data_Error;
   end Get;

   ---------
   -- Put --
   ---------

   procedure Put
     (File  : File_Type;
      Item  : Enum;
      Width : Field := Default_Width;
      Set   : Type_Set := Default_Setting)
   is
   begin
      --  Ensure that Item is valid before attempting to retrieve the Image, to
      --  prevent the possibility of out-of-bounds addressing of index or image
      --  tables. Units in the run-time library are normally compiled with
      --  checks suppressed, which includes instantiated generics.

      if not Item'Valid then
         raise Constraint_Error with "invalid enumeration value";
      end if;

      Aux.Put (File, Enum'Image (Item), Width, Set);
   end Put;

   procedure Put
     (Item  : Enum;
      Width : Field := Default_Width;
      Set   : Type_Set := Default_Setting)
   is
   begin
      Put (Current_Out, Item, Width, Set);
   end Put;

   procedure Put
     (To   : out String;
      Item : Enum;
      Set  : Type_Set := Default_Setting)
   is
   begin
      --  Ensure that Item is valid before attempting to retrieve the Image, to
      --  prevent the possibility of out-of-bounds addressing of index or image
      --  tables. Units in the run-time library are normally compiled with
      --  checks suppressed, which includes instantiated generics.

      if not Item'Valid then
         raise Constraint_Error with "invalid enumeration value";
      end if;

      Aux.Puts (To, Enum'Image (Item), Set);
   end Put;

end Ada.Text_IO.Enumeration_IO;

------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--           A D A . T E X T _ I O . E N U M E R A T I O N _ I O            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2005, Free Software Foundation, Inc.         --
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

with Ada.Text_IO.Enumeration_Aux;

package body Ada.Text_IO.Enumeration_IO is

   package Aux renames Ada.Text_IO.Enumeration_Aux;

   ---------
   -- Get --
   ---------

   procedure Get (File : in File_Type; Item : out Enum) is
      Buf    : String (1 .. Enum'Width);
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
     (From : in String;
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
     (File  : in File_Type;
      Item  : in Enum;
      Width : in Field := Default_Width;
      Set   : in Type_Set := Default_Setting)
   is
      Image : constant String := Enum'Image (Item);

   begin
      Aux.Put (File, Image, Width, Set);
   end Put;

   procedure Put
     (Item  : in Enum;
      Width : in Field := Default_Width;
      Set   : in Type_Set := Default_Setting)
   is
   begin
      Put (Current_Out, Item, Width, Set);
   end Put;

   procedure Put
     (To   : out String;
      Item : in Enum;
      Set  : in Type_Set := Default_Setting)
   is
      Image : constant String := Enum'Image (Item);

   begin
      Aux.Puts (To, Image, Set);
   end Put;

end Ada.Text_IO.Enumeration_IO;

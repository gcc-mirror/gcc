------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--    A D A . W I D E _ W I D E _ T E X T _ I O . I N T E G E R  _ A U X    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2005 Free Software Foundation, Inc.          --
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

with Ada.Wide_Wide_Text_IO.Generic_Aux; use Ada.Wide_Wide_Text_IO.Generic_Aux;

with System.Img_BIU;   use System.Img_BIU;
with System.Img_Int;   use System.Img_Int;
with System.Img_LLB;   use System.Img_LLB;
with System.Img_LLI;   use System.Img_LLI;
with System.Img_LLW;   use System.Img_LLW;
with System.Img_WIU;   use System.Img_WIU;
with System.Val_Int;   use System.Val_Int;
with System.Val_LLI;   use System.Val_LLI;

package body Ada.Wide_Wide_Text_IO.Integer_Aux is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Load_Integer
     (File : File_Type;
      Buf  : out String;
      Ptr  : in out Natural);
   --  This is an auxiliary routine that is used to load an possibly signed
   --  integer literal value from the input file into Buf, starting at Ptr + 1.
   --  On return, Ptr is set to the last character stored.

   -------------
   -- Get_Int --
   -------------

   procedure Get_Int
     (File  : File_Type;
      Item  : out Integer;
      Width : Field)
   is
      Buf  : String (1 .. Field'Last);
      Ptr  : aliased Integer := 1;
      Stop : Integer := 0;

   begin
      if Width /= 0 then
         Load_Width (File, Width, Buf, Stop);
         String_Skip (Buf, Ptr);
      else
         Load_Integer (File, Buf, Stop);
      end if;

      Item := Scan_Integer (Buf, Ptr'Access, Stop);
      Check_End_Of_Field (Buf, Stop, Ptr, Width);
   end Get_Int;

   -------------
   -- Get_LLI --
   -------------

   procedure Get_LLI
     (File  : File_Type;
      Item  : out Long_Long_Integer;
      Width : Field)
   is
      Buf  : String (1 .. Field'Last);
      Ptr  : aliased Integer := 1;
      Stop : Integer := 0;

   begin
      if Width /= 0 then
         Load_Width (File, Width, Buf, Stop);
         String_Skip (Buf, Ptr);
      else
         Load_Integer (File, Buf, Stop);
      end if;

      Item := Scan_Long_Long_Integer (Buf, Ptr'Access, Stop);
      Check_End_Of_Field (Buf, Stop, Ptr, Width);
   end Get_LLI;

   --------------
   -- Gets_Int --
   --------------

   procedure Gets_Int
     (From : String;
      Item : out Integer;
      Last : out Positive)
   is
      Pos : aliased Integer;

   begin
      String_Skip (From, Pos);
      Item := Scan_Integer (From, Pos'Access, From'Last);
      Last := Pos - 1;

   exception
      when Constraint_Error =>
         raise Data_Error;
   end Gets_Int;

   --------------
   -- Gets_LLI --
   --------------

   procedure Gets_LLI
     (From : String;
      Item : out Long_Long_Integer;
      Last : out Positive)
   is
      Pos : aliased Integer;

   begin
      String_Skip (From, Pos);
      Item := Scan_Long_Long_Integer (From, Pos'Access, From'Last);
      Last := Pos - 1;

   exception
      when Constraint_Error =>
         raise Data_Error;
   end Gets_LLI;

   ------------------
   -- Load_Integer --
   ------------------

   procedure Load_Integer
     (File : File_Type;
      Buf  : out String;
      Ptr  : in out Natural)
   is
      Hash_Loc : Natural;
      Loaded   : Boolean;

   begin
      Load_Skip (File);
      Load (File, Buf, Ptr, '+', '-');

      Load_Digits (File, Buf, Ptr, Loaded);

      if Loaded then
         Load (File, Buf, Ptr, '#', ':', Loaded);

         if Loaded then
            Hash_Loc := Ptr;
            Load_Extended_Digits (File, Buf, Ptr);
            Load (File, Buf, Ptr, Buf (Hash_Loc));
         end if;

         Load (File, Buf, Ptr, 'E', 'e', Loaded);

         if Loaded then

            --  Note: it is strange to allow a minus sign, since the syntax
            --  does not, but that is what ACVC test CE3704F, case (6) wants.

            Load (File, Buf, Ptr, '+', '-');
            Load_Digits (File, Buf, Ptr);
         end if;
      end if;
   end Load_Integer;

   -------------
   -- Put_Int --
   -------------

   procedure Put_Int
     (File  : File_Type;
      Item  : Integer;
      Width : Field;
      Base  : Number_Base)
   is
      Buf : String (1 .. Field'Last);
      Ptr : Natural := 0;

   begin
      if Base = 10 and then Width = 0 then
         Set_Image_Integer (Item, Buf, Ptr);
      elsif Base = 10 then
         Set_Image_Width_Integer (Item, Width, Buf, Ptr);
      else
         Set_Image_Based_Integer (Item, Base, Width, Buf, Ptr);
      end if;

      Put_Item (File, Buf (1 .. Ptr));
   end Put_Int;

   -------------
   -- Put_LLI --
   -------------

   procedure Put_LLI
     (File  : File_Type;
      Item  : Long_Long_Integer;
      Width : Field;
      Base  : Number_Base)
   is
      Buf : String (1 .. Field'Last);
      Ptr : Natural := 0;

   begin
      if Base = 10 and then Width = 0 then
         Set_Image_Long_Long_Integer (Item, Buf, Ptr);
      elsif Base = 10 then
         Set_Image_Width_Long_Long_Integer (Item, Width, Buf, Ptr);
      else
         Set_Image_Based_Long_Long_Integer (Item, Base, Width, Buf, Ptr);
      end if;

      Put_Item (File, Buf (1 .. Ptr));
   end Put_LLI;

   --------------
   -- Puts_Int --
   --------------

   procedure Puts_Int
     (To   : out String;
      Item : Integer;
      Base : Number_Base)
   is
      Buf : String (1 .. Field'Last);
      Ptr : Natural := 0;

   begin
      if Base = 10 then
         Set_Image_Width_Integer (Item, To'Length, Buf, Ptr);
      else
         Set_Image_Based_Integer (Item, Base, To'Length, Buf, Ptr);
      end if;

      if Ptr > To'Length then
         raise Layout_Error;
      else
         To (To'First .. To'First + Ptr - 1) := Buf (1 .. Ptr);
      end if;
   end Puts_Int;

   --------------
   -- Puts_LLI --
   --------------

   procedure Puts_LLI
     (To   : out String;
      Item : Long_Long_Integer;
      Base : Number_Base)
   is
      Buf : String (1 .. Field'Last);
      Ptr : Natural := 0;

   begin
      if Base = 10 then
         Set_Image_Width_Long_Long_Integer (Item, To'Length, Buf, Ptr);
      else
         Set_Image_Based_Long_Long_Integer (Item, Base, To'Length, Buf, Ptr);
      end if;

      if Ptr > To'Length then
         raise Layout_Error;
      else
         To (To'First .. To'First + Ptr - 1) := Buf (1 .. Ptr);
      end if;
   end Puts_LLI;

end Ada.Wide_Wide_Text_IO.Integer_Aux;

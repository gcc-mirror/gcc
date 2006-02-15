------------------------------------------------------------------------------
--                                                                          --
--                        GNAT RUN-TIME COMPONENTS                          --
--                                                                          --
--                A D A . T E X T _ I O . F L O A T _ A U X                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2006, Free Software Foundation, Inc.         --
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

with Ada.Text_IO.Generic_Aux; use Ada.Text_IO.Generic_Aux;

with System.Img_Real; use System.Img_Real;
with System.Val_Real; use System.Val_Real;

package body Ada.Text_IO.Float_Aux is

   ---------
   -- Get --
   ---------

   procedure Get
     (File  : File_Type;
      Item  : out Long_Long_Float;
      Width : Field)
   is
      Buf  : String (1 .. Field'Last);
      Stop : Integer := 0;
      Ptr  : aliased Integer := 1;

   begin
      if Width /= 0 then
         Load_Width (File, Width, Buf, Stop);
         String_Skip (Buf, Ptr);
      else
         Load_Real (File, Buf, Stop);
      end if;

      Item := Scan_Real (Buf, Ptr'Access, Stop);

      Check_End_Of_Field (Buf, Stop, Ptr, Width);
   end Get;

   ----------
   -- Gets --
   ----------

   procedure Gets
     (From : String;
      Item : out Long_Long_Float;
      Last : out Positive)
   is
      Pos : aliased Integer;

   begin
      String_Skip (From, Pos);
      Item := Scan_Real (From, Pos'Access, From'Last);
      Last := Pos - 1;

   exception
      when Constraint_Error =>
         raise Data_Error;
   end Gets;

   ---------------
   -- Load_Real --
   ---------------

   procedure Load_Real
     (File : File_Type;
      Buf  : out String;
      Ptr  : in out Natural)
   is
      Loaded   : Boolean;

   begin
      --  Skip initial blanks, and load possible sign

      Load_Skip (File);
      Load (File, Buf, Ptr, '+', '-');

      --  Case of .nnnn

      Load (File, Buf, Ptr, '.', Loaded);

      if Loaded then
         Load_Digits (File, Buf, Ptr, Loaded);

         --  Hopeless junk if no digits loaded

         if not Loaded then
            return;
         end if;

      --  Otherwise must have digits to start

      else
         Load_Digits (File, Buf, Ptr, Loaded);

         --  Hopeless junk if no digits loaded

         if not Loaded then
            return;
         end if;

         --  Based cases

         Load (File, Buf, Ptr, '#', ':', Loaded);

         if Loaded then

            --  Case of nnn#.xxx#

            Load (File, Buf, Ptr, '.', Loaded);

            if Loaded then
               Load_Extended_Digits (File, Buf, Ptr);

            --  Case of nnn#xxx.[xxx]# or nnn#xxx#

            else
               Load_Extended_Digits (File, Buf, Ptr);
               Load (File, Buf, Ptr, '.', Loaded);

               if Loaded then
                  Load_Extended_Digits (File, Buf, Ptr);
               end if;

               --  As usual, it seems strange to allow mixed base characters,
               --  but that is what ACVC tests expect, see CE3804M, case (3).

               Load (File, Buf, Ptr, '#', ':');
            end if;

         --  Case of nnn.[nnn] or nnn

         else
            Load (File, Buf, Ptr, '.', Loaded);

            if Loaded then
               Load_Digits (File, Buf, Ptr);
            end if;
         end if;
      end if;

      --  Deal with exponent

      Load (File, Buf, Ptr, 'E', 'e', Loaded);

      if Loaded then
         Load (File, Buf, Ptr, '+', '-');
         Load_Digits (File, Buf, Ptr);
      end if;
   end Load_Real;

   ---------
   -- Put --
   ---------

   procedure Put
     (File : File_Type;
      Item : Long_Long_Float;
      Fore : Field;
      Aft  : Field;
      Exp  : Field)
   is
      Buf : String (1 .. 3 * Field'Last + 2);
      Ptr : Natural := 0;

   begin
      Set_Image_Real (Item, Buf, Ptr, Fore, Aft, Exp);
      Put_Item (File, Buf (1 .. Ptr));
   end Put;

   ----------
   -- Puts --
   ----------

   procedure Puts
     (To   : out String;
      Item : Long_Long_Float;
      Aft  : Field;
      Exp  : Field)
   is
      Buf : String (1 .. 3 * Field'Last + 2);
      Ptr : Natural := 0;

   begin
      Set_Image_Real (Item, Buf, Ptr, Fore => 1, Aft => Aft, Exp => Exp);

      if Ptr > To'Length then
         raise Layout_Error;

      else
         for J in 1 .. Ptr loop
            To (To'Last - Ptr + J) := Buf (J);
         end loop;

         for J in To'First .. To'Last - Ptr loop
            To (J) := ' ';
         end loop;
      end if;
   end Puts;

end Ada.Text_IO.Float_Aux;

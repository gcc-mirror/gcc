------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--           A D A . W I D E _ T E X T _ I O . D E C I M A L _ I O          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2003 Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
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

with Ada.Wide_Text_IO.Decimal_Aux;
with System.WCh_Con; use System.WCh_Con;
with System.WCh_WtS; use System.WCh_WtS;

package body Ada.Wide_Text_IO.Decimal_IO is

   subtype TFT is Ada.Wide_Text_IO.File_Type;
   --  File type required for calls to routines in Aux

   package Aux renames Ada.Wide_Text_IO.Decimal_Aux;

   Scale : constant Integer := Num'Scale;

   ---------
   -- Get --
   ---------

   procedure Get
     (File  : in File_Type;
      Item  : out Num;
      Width : in Field := 0)
   is
   begin
      if Num'Size > Integer'Size then
         Item := Num (Aux.Get_LLD (TFT (File), Width, Scale));
         --  Item := Num'Fixed_Value (Aux.Get_LLD (TFT (File), Width, Scale));
         --  above is what we should write, but gets assert error ???

      else
         Item := Num (Aux.Get_Dec (TFT (File), Width, Scale));
         --  Item := Num'Fixed_Value (Aux.Get_Dec (TFT (File), Width, Scale));
         --  above is what we should write, but gets assert error ???
      end if;

   exception
      when Constraint_Error => raise Data_Error;
   end Get;

   procedure Get
     (Item  : out Num;
      Width : in Field := 0)
   is
   begin
      Get (Current_Input, Item, Width);
   end Get;

   procedure Get
     (From : in Wide_String;
      Item : out Num;
      Last : out Positive)
   is
      S : constant String := Wide_String_To_String (From, WCEM_Upper);
      --  String on which we do the actual conversion. Note that the method
      --  used for wide character encoding is irrelevant, since if there is
      --  a character outside the Standard.Character range then the call to
      --  Aux.Gets will raise Data_Error in any case.

   begin
      if Num'Size > Integer'Size then
         --  Item := Num'Fixed_Value
         --  should write above, but gets assert error ???
         Item := Num
                   (Aux.Gets_LLD (S, Last'Unrestricted_Access, Scale));
      else
         --  Item := Num'Fixed_Value
         --  should write above, but gets assert error ???
         Item := Num
                   (Aux.Gets_Dec (S, Last'Unrestricted_Access, Scale));
      end if;

   exception
      when Constraint_Error => raise Data_Error;
   end Get;

   ---------
   -- Put --
   ---------

   procedure Put
     (File : in File_Type;
      Item : in Num;
      Fore : in Field := Default_Fore;
      Aft  : in Field := Default_Aft;
      Exp  : in Field := Default_Exp)
   is
   begin
      if Num'Size > Integer'Size then
         Aux.Put_LLD
--           (TFT (File), Long_Long_Integer'Integer_Value (Item),
--  ???
           (TFT (File), Long_Long_Integer (Item),
            Fore, Aft, Exp, Scale);
      else
         Aux.Put_Dec
--           (TFT (File), Integer'Integer_Value (Item), Fore, Aft, Exp, Scale);
--  ???
           (TFT (File), Integer (Item), Fore, Aft, Exp, Scale);

      end if;
   end Put;

   procedure Put
     (Item : in Num;
      Fore : in Field := Default_Fore;
      Aft  : in Field := Default_Aft;
      Exp  : in Field := Default_Exp)
   is
      pragma Unreferenced (Fore);
      --  ??? how come this is unreferenced, sounds wrong ???
   begin
      Put (Current_Output, Item, Aft, Exp);
   end Put;

   procedure Put
     (To   : out Wide_String;
      Item : in Num;
      Aft  : in Field := Default_Aft;
      Exp  : in Field := Default_Exp)
   is
      S : String (To'First .. To'Last);

   begin
      if Num'Size > Integer'Size then
--       Aux.Puts_LLD
--         (S, Long_Long_Integer'Integer_Value (Item), Aft, Exp, Scale);
--  ???
         Aux.Puts_LLD
           (S, Long_Long_Integer (Item), Aft, Exp, Scale);
      else
--       Aux.Puts_Dec (S, Integer'Integer_Value (Item), Aft, Exp, Scale);
--  ???
         Aux.Puts_Dec (S, Integer (Item), Aft, Exp, Scale);
      end if;

      for J in S'Range loop
         To (J) := Wide_Character'Val (Character'Pos (S (J)));
      end loop;
   end Put;

end Ada.Wide_Text_IO.Decimal_IO;

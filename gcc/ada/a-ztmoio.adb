------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--      A D A . W I D E _ W I D E _ T E X T _ I O . M O D U L A R _ I O     --
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

with Ada.Wide_Wide_Text_IO.Modular_Aux;

with System.Unsigned_Types; use System.Unsigned_Types;
with System.WCh_Con;        use System.WCh_Con;
with System.WCh_WtS;        use System.WCh_WtS;

package body Ada.Wide_Wide_Text_IO.Modular_IO is

   subtype TFT is Ada.Wide_Wide_Text_IO.File_Type;
   --  File type required for calls to routines in Aux

   package Aux renames Ada.Wide_Wide_Text_IO.Modular_Aux;

   ---------
   -- Get --
   ---------

   procedure Get
     (File  : File_Type;
      Item  : out Num;
      Width : Field := 0)
   is
   begin
      if Num'Size > Unsigned'Size then
         Aux.Get_LLU (TFT (File), Long_Long_Unsigned (Item), Width);
      else
         Aux.Get_Uns (TFT (File), Unsigned (Item), Width);
      end if;

   exception
      when Constraint_Error => raise Data_Error;
   end Get;

   procedure Get
     (Item  : out Num;
      Width : Field := 0)
   is
   begin
      Get (Current_Input, Item, Width);
   end Get;

   procedure Get
     (From : Wide_Wide_String;
      Item : out Num;
      Last : out Positive)
   is
      S : constant String := Wide_Wide_String_To_String (From, WCEM_Upper);
      --  String on which we do the actual conversion. Note that the method
      --  used for wide character encoding is irrelevant, since if there is
      --  a character outside the Standard.Character range then the call to
      --  Aux.Gets will raise Data_Error in any case.

   begin
      if Num'Size > Unsigned'Size then
         Aux.Gets_LLU (S, Long_Long_Unsigned (Item), Last);
      else
         Aux.Gets_Uns (S, Unsigned (Item), Last);
      end if;

   exception
      when Constraint_Error => raise Data_Error;
   end Get;

   ---------
   -- Put --
   ---------

   procedure Put
     (File  : File_Type;
      Item  : Num;
      Width : Field := Default_Width;
      Base  : Number_Base := Default_Base)
   is
   begin
      if Num'Size > Unsigned'Size then
         Aux.Put_LLU (TFT (File), Long_Long_Unsigned (Item), Width, Base);
      else
         Aux.Put_Uns (TFT (File), Unsigned (Item), Width, Base);
      end if;
   end Put;

   procedure Put
     (Item  : Num;
      Width : Field := Default_Width;
      Base  : Number_Base := Default_Base)
   is
   begin
      Put (Current_Output, Item, Width, Base);
   end Put;

   procedure Put
     (To   : out Wide_Wide_String;
      Item : Num;
      Base : Number_Base := Default_Base)
   is
      S : String (To'First .. To'Last);

   begin
      if Num'Size > Unsigned'Size then
         Aux.Puts_LLU (S, Long_Long_Unsigned (Item), Base);
      else
         Aux.Puts_Uns (S, Unsigned (Item), Base);
      end if;

      for J in S'Range loop
         To (J) := Wide_Wide_Character'Val (Character'Pos (S (J)));
      end loop;
   end Put;

end Ada.Wide_Wide_Text_IO.Modular_IO;

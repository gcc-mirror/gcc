------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--           A D A . W I D E _ T E X T _ I O . I N T E G E R _ I O          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2019, Free Software Foundation, Inc.         --
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

with Ada.Wide_Text_IO.Integer_Aux;
with System.WCh_Con; use System.WCh_Con;
with System.WCh_WtS; use System.WCh_WtS;

package body Ada.Wide_Text_IO.Integer_IO is

   Need_LLI : constant Boolean := Num'Base'Size > Integer'Size;
   --  Throughout this generic body, we distinguish between the case where type
   --  Integer is acceptable, and where a Long_Long_Integer is needed. This
   --  Boolean is used to test for these cases and since it is a constant, only
   --  code for the relevant case will be included in the instance.

   subtype TFT is Ada.Wide_Text_IO.File_Type;
   --  File type required for calls to routines in Aux

   package Aux renames Ada.Wide_Text_IO.Integer_Aux;

   ---------
   -- Get --
   ---------

   procedure Get
     (File  : File_Type;
      Item  : out Num;
      Width : Field := 0)
   is
   begin
      if Need_LLI then
         Aux.Get_LLI (TFT (File), Long_Long_Integer (Item), Width);
      else
         Aux.Get_Int (TFT (File), Integer (Item), Width);
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
     (From : Wide_String;
      Item : out Num;
      Last : out Positive)
   is
      S : constant String := Wide_String_To_String (From, WCEM_Upper);
      --  String on which we do the actual conversion. Note that the method
      --  used for wide character encoding is irrelevant, since if there is
      --  a character outside the Standard.Character range then the call to
      --  Aux.Gets will raise Data_Error in any case.

   begin
      if Need_LLI then
         Aux.Gets_LLI (S, Long_Long_Integer (Item), Last);
      else
         Aux.Gets_Int (S, Integer (Item), Last);
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
      if Need_LLI then
         Aux.Put_LLI (TFT (File), Long_Long_Integer (Item), Width, Base);
      else
         Aux.Put_Int (TFT (File), Integer (Item), Width, Base);
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
     (To   : out Wide_String;
      Item : Num;
      Base : Number_Base := Default_Base)
   is
      S : String (To'First .. To'Last);

   begin
      if Need_LLI then
         Aux.Puts_LLI (S, Long_Long_Integer (Item), Base);
      else
         Aux.Puts_Int (S, Integer (Item), Base);
      end if;

      for J in S'Range loop
         To (J) := Wide_Character'Val (Character'Pos (S (J)));
      end loop;
   end Put;

end Ada.Wide_Text_IO.Integer_IO;

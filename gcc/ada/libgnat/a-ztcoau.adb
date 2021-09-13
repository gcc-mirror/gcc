------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--    A D A . W I D E _ W I D E _ T E X T _ I O . C O M P L E X _ A U X     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2021, Free Software Foundation, Inc.         --
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

with Ada.Wide_Wide_Text_IO.Generic_Aux; use Ada.Wide_Wide_Text_IO.Generic_Aux;

package body Ada.Wide_Wide_Text_IO.Complex_Aux is

   ---------
   -- Get --
   ---------

   procedure Get
     (File  : File_Type;
      ItemR : out Num;
      ItemI : out Num;
      Width : Field)
   is
      Buf   : String (1 .. Field'Last);
      Stop  : Integer := 0;
      Ptr   : aliased Integer;
      Paren : Boolean := False;

   begin
      --  General note for following code, exceptions from the calls
      --  to Get for components of the complex value are propagated.

      if Width /= 0 then
         Load_Width (File, Width, Buf, Stop);
         Gets (Buf (1 .. Stop), ItemR, ItemI, Ptr);

         for J in Ptr + 1 .. Stop loop
            if not Is_Blank (Buf (J)) then
               raise Data_Error;
            end if;
         end loop;

      --  Case of width = 0

      else
         Load_Skip (File);
         Ptr := 0;
         Load (File, Buf, Ptr, '(', Paren);
         Aux.Get (File, ItemR, 0);
         Load_Skip (File);
         Load (File, Buf, Ptr, ',');
         Aux.Get (File, ItemI, 0);

         if Paren then
            Load_Skip (File);
            Load (File, Buf, Ptr, ')', Paren);

            if not Paren then
               raise Data_Error;
            end if;
         end if;
      end if;
   end Get;

   ----------
   -- Gets --
   ----------

   procedure Gets
     (From  : String;
      ItemR : out Num;
      ItemI : out Num;
      Last  : out Positive)
   is
      Paren : Boolean;
      Pos   : Integer;

   begin
      String_Skip (From, Pos);

      if From (Pos) = '(' then
         Pos := Pos + 1;
         Paren := True;
      else
         Paren := False;
      end if;

      Aux.Gets (From (Pos .. From'Last), ItemR, Pos);

      String_Skip (From (Pos + 1 .. From'Last), Pos);

      if From (Pos) = ',' then
         Pos := Pos + 1;
      end if;

      Aux.Gets (From (Pos .. From'Last), ItemI, Pos);

      if Paren then
         String_Skip (From (Pos + 1 .. From'Last), Pos);

         if From (Pos) /= ')' then
            raise Data_Error;
         end if;
      end if;

      Last := Pos;
   end Gets;

   ---------
   -- Put --
   ---------

   procedure Put
     (File  : File_Type;
      ItemR : Num;
      ItemI : Num;
      Fore  : Field;
      Aft   : Field;
      Exp   : Field)
   is
   begin
      Put (File, '(');
      Aux.Put (File, ItemR, Fore, Aft, Exp);
      Put (File, ',');
      Aux.Put (File, ItemI, Fore, Aft, Exp);
      Put (File, ')');
   end Put;

   ----------
   -- Puts --
   ----------

   procedure Puts
     (To    : out String;
      ItemR : Num;
      ItemI : Num;
      Aft   :  Field;
      Exp   :  Field)
   is
      I_String : String (1 .. 3 * Field'Last);
      R_String : String (1 .. 3 * Field'Last);

      Iptr : Natural;
      Rptr : Natural;

   begin
      --  Both parts are initially converted with a Fore of 0

      Rptr := 0;
      Aux.Set_Image (ItemR, R_String, Rptr, 0, Aft, Exp);
      Iptr := 0;
      Aux.Set_Image (ItemI, I_String, Iptr, 0, Aft, Exp);

      --  Check room for both parts plus parens plus comma (RM G.1.3(34))

      if Rptr + Iptr + 3 > To'Length then
         raise Layout_Error;
      end if;

      --  If there is room, layout result according to (RM G.1.3(31-33))

      To (To'First) := '(';
      To (To'First + 1 .. To'First + Rptr) := R_String (1 .. Rptr);
      To (To'First + Rptr + 1) := ',';

      To (To'Last) := ')';

      To (To'Last - Iptr .. To'Last - 1) := I_String (1 .. Iptr);

      for J in To'First + Rptr + 2 .. To'Last - Iptr - 1 loop
         To (J) := ' ';
      end loop;
   end Puts;

end Ada.Wide_Wide_Text_IO.Complex_Aux;

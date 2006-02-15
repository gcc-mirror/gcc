------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               S W I T C H                                --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Osint;

package body Switch is

   ----------------
   -- Bad_Switch --
   ----------------

   procedure Bad_Switch (Switch : Character) is
   begin
      Osint.Fail ("invalid switch: ", (1 => Switch));
   end Bad_Switch;

   procedure Bad_Switch (Switch : String) is
   begin
      Osint.Fail ("invalid switch: ", Switch);
   end Bad_Switch;

   -------------------------
   -- Is_Front_End_Switch --
   -------------------------

   function Is_Front_End_Switch (Switch_Chars : String) return Boolean is
      Ptr : constant Positive := Switch_Chars'First;
   begin
      return Is_Switch (Switch_Chars)
        and then
          (Switch_Chars (Ptr + 1) = 'I'
            or else (Switch_Chars'Length >= 5
                      and then Switch_Chars (Ptr + 1 .. Ptr + 4) = "gnat")
            or else (Switch_Chars'Length >= 5
                      and then Switch_Chars (Ptr + 2 .. Ptr + 4) = "RTS"));
   end Is_Front_End_Switch;

   ---------------
   -- Is_Switch --
   ---------------

   function Is_Switch (Switch_Chars : String) return Boolean is
   begin
      return Switch_Chars'Length > 1
        and then Switch_Chars (Switch_Chars'First) = '-';
   end Is_Switch;

   --------------
   -- Scan_Nat --
   --------------

   procedure Scan_Nat
     (Switch_Chars : String;
      Max          : Integer;
      Ptr          : in out Integer;
      Result       : out Nat;
      Switch       : Character)
   is
   begin
      Result := 0;

      if Ptr > Max or else Switch_Chars (Ptr) not in '0' .. '9' then
         Osint.Fail ("missing numeric value for switch: ", (1 => Switch));

      else
         while Ptr <= Max and then Switch_Chars (Ptr) in '0' .. '9' loop
            Result := Result * 10 +
              Character'Pos (Switch_Chars (Ptr)) - Character'Pos ('0');
            Ptr := Ptr + 1;

            if Result > Switch_Max_Value then
               Osint.Fail
                 ("numeric value out of range for switch: ", (1 => Switch));
            end if;
         end loop;
      end if;
   end Scan_Nat;

   --------------
   -- Scan_Pos --
   --------------

   procedure Scan_Pos
     (Switch_Chars : String;
      Max          : Integer;
      Ptr          : in out Integer;
      Result       : out Pos;
      Switch       : Character)
   is
      Temp : Nat;

   begin
      Scan_Nat (Switch_Chars, Max, Ptr, Temp, Switch);

      if Temp = 0 then
         Osint.Fail ("numeric value out of range for switch: ", (1 => Switch));
      end if;

      Result := Temp;
   end Scan_Pos;

end Switch;

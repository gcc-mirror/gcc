------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--              A D A . S T R I N G S . U T F _ E N C O D I N G             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 2010, Free Software Foundation, Inc.           --
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

package body Ada.Strings.UTF_Encoding is
   use Interfaces;

   --------------
   -- Encoding --
   --------------

   function Encoding
     (Item    : UTF_String;
      Default : Encoding_Scheme := UTF_8) return Encoding_Scheme
   is
   begin
      if Item'Length >= 2 then
         if Item (Item'First .. Item'First + 1) = BOM_16BE then
            return UTF_16BE;

         elsif Item (Item'First .. Item'First + 1) = BOM_16LE then
            return UTF_16LE;

         elsif Item'Length >= 3
           and then Item (Item'First .. Item'First + 2) = BOM_8
         then
            return UTF_8;
         end if;
      end if;

      return Default;
   end Encoding;

   -----------------
   -- From_UTF_16 --
   -----------------

   function From_UTF_16
     (Item          : UTF_16_Wide_String;
      Output_Scheme : UTF_XE_Encoding;
      Output_BOM    : Boolean := False) return UTF_String
   is
      BSpace : constant Natural := 2 * Boolean'Pos (Output_BOM);
      Result : UTF_String (1 .. 2 * Item'Length + BSpace);
      Len    : Natural;
      C      : Unsigned_16;
      Iptr   : Natural;

   begin
      if Output_BOM then
         Result (1 .. 2) :=
           (if Output_Scheme = UTF_16BE then BOM_16BE else BOM_16LE);
         Len := 2;
      else
         Len := 0;
      end if;

      --  Skip input BOM

      Iptr := Item'First;

      if Iptr <= Item'Last and then Item (Iptr) = BOM_16 (1) then
         Iptr := Iptr + 1;
      end if;

      --  UTF-16BE case

      if Output_Scheme = UTF_16BE then
         while Iptr <= Item'Last loop
            C := To_Unsigned_16 (Item (Iptr));
            Result (Len + 1) := Character'Val (Shift_Right (C, 8));
            Result (Len + 2) := Character'Val (C and 16#00_FF#);
            Len := Len + 2;
            Iptr := Iptr + 1;
         end loop;

      --  UTF-16LE case

      else
         while Iptr <= Item'Last loop
            C := To_Unsigned_16 (Item (Iptr));
            Result (Len + 1) := Character'Val (C and 16#00_FF#);
            Result (Len + 2) := Character'Val (Shift_Right (C, 8));
            Len := Len + 2;
            Iptr := Iptr + 1;
         end loop;
      end if;

      return Result (1 .. Len);
   end From_UTF_16;

   --------------------------
   -- Raise_Encoding_Error --
   --------------------------

   procedure Raise_Encoding_Error (Index : Natural) is
      Val : constant String := Index'Img;
   begin
      raise Encoding_Error with
        "bad input at Item (" & Val (Val'First + 1 .. Val'Last) & ')';
   end Raise_Encoding_Error;

   ---------------
   -- To_UTF_16 --
   ---------------

   function To_UTF_16
     (Item         : UTF_String;
      Input_Scheme : UTF_XE_Encoding;
      Output_BOM   : Boolean := False) return UTF_16_Wide_String
   is
      Result : UTF_16_Wide_String (1 .. Item'Length / 2 + 1);
      Len    : Natural;
      Iptr   : Natural;

   begin
      if Item'Length mod 2 /= 0 then
         raise Encoding_Error with "UTF-16BE/LE string has odd length";
      end if;

      --  Deal with input BOM, skip if OK, error if bad BOM

      Iptr := Item'First;

      if Item'Length >= 2 then
         if Item (Iptr .. Iptr + 1) = BOM_16BE then
            if Input_Scheme = UTF_16BE then
               Iptr := Iptr + 2;
            else
               Raise_Encoding_Error (Iptr);
            end if;

         elsif Item (Iptr .. Iptr + 1) = BOM_16LE then
            if Input_Scheme = UTF_16LE then
               Iptr := Iptr + 2;
            else
               Raise_Encoding_Error (Iptr);
            end if;

         elsif Item'Length >= 3 and then Item (Iptr .. Iptr + 2) = BOM_8 then
            Raise_Encoding_Error (Iptr);
         end if;
      end if;

      --  Output BOM if specified

      if Output_BOM then
         Result (1) := BOM_16 (1);
         Len := 1;
      else
         Len := 0;
      end if;

      --  UTF-16BE case

      if Input_Scheme = UTF_16BE then
         while Iptr < Item'Last loop
            Len := Len + 1;
            Result (Len) :=
              Wide_Character'Val
                (Character'Pos (Item (Iptr)) * 256 +
                   Character'Pos (Item (Iptr + 1)));
            Iptr := Iptr + 2;
         end loop;

      --  UTF-16LE case

      else
         while Iptr < Item'Last loop
            Len := Len + 1;
            Result (Len) :=
              Wide_Character'Val
                (Character'Pos (Item (Iptr)) +
                 Character'Pos (Item (Iptr + 1)) * 256);
            Iptr := Iptr + 2;
         end loop;
      end if;

      return Result (1 .. Len);
   end To_UTF_16;

end Ada.Strings.UTF_Encoding;

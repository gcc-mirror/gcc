------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                S Y S T E M . B O U N D E D _ S T R I N G S               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                       Copyright (C) 2016-2018, AdaCore                   --
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

with System.Storage_Elements;

package body System.Bounded_Strings is

   ------------
   -- Append --
   ------------

   procedure Append (X : in out Bounded_String; C : Character) is
   begin
      --  If we have too many characters to fit, simply drop them

      if X.Length < X.Max_Length then
         X.Length           := X.Length + 1;
         X.Chars (X.Length) := C;
      end if;
   end Append;

   procedure Append (X : in out Bounded_String; S : String) is
   begin
      for C of S loop
         Append (X, C);
      end loop;
   end Append;

   --------------------
   -- Append_Address --
   --------------------

   procedure Append_Address (X : in out Bounded_String; A : Address)
   is
      S : String (1 .. 18);
      P : Natural;
      use System.Storage_Elements;
      N : Integer_Address;

      H : constant array (Integer range 0 .. 15) of Character :=
        "0123456789abcdef";
   begin
      P := S'Last;
      N := To_Integer (A);
      loop
         S (P) := H (Integer (N mod 16));
         P := P - 1;
         N := N / 16;
         exit when N = 0;
      end loop;

      S (P - 1) := '0';
      S (P) := 'x';

      Append (X, S (P - 1 .. S'Last));
   end Append_Address;

   -------------
   -- Is_Full --
   -------------

   function Is_Full (X : Bounded_String) return Boolean is
   begin
      return X.Length >= X.Max_Length;
   end Is_Full;

   ---------------
   -- To_String --
   ---------------

   function To_String (X : Bounded_String) return String is
   begin
      return X.Chars (1 .. X.Length);
   end To_String;

end System.Bounded_Strings;

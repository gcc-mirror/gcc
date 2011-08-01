------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                           G N A T . C R C 3 2                            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2001-2010, AdaCore                     --
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

with Ada.Unchecked_Conversion;

package body GNAT.CRC32 is

   ------------
   -- Update --
   ------------

   procedure Update (C : in out CRC32; Value : String) is
   begin
      for K in Value'Range loop
         Update (C, Value (K));
      end loop;
   end Update;

   procedure Update (C : in out CRC32; Value : Ada.Streams.Stream_Element) is
      function To_Char is new Ada.Unchecked_Conversion
        (Ada.Streams.Stream_Element, Character);
      V : constant Character := To_Char (Value);
   begin
      Update (C, V);
   end Update;

   procedure Update
     (C     : in out CRC32;
      Value : Ada.Streams.Stream_Element_Array)
   is
   begin
      for K in Value'Range loop
         Update (C, Value (K));
      end loop;
   end Update;

   -----------------
   -- Wide_Update --
   -----------------

   procedure Wide_Update (C : in out CRC32; Value : Wide_Character) is
      subtype S2 is String (1 .. 2);
      function To_S2 is new Ada.Unchecked_Conversion (Wide_Character, S2);
      VS : constant S2 := To_S2 (Value);
   begin
      Update (C, VS (1));
      Update (C, VS (2));
   end Wide_Update;

   procedure Wide_Update (C : in out CRC32; Value : Wide_String) is
   begin
      for K in Value'Range loop
         Wide_Update (C, Value (K));
      end loop;
   end Wide_Update;

end GNAT.CRC32;

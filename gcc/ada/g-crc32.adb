------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                           G N A T . C R C 3 2                            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2001-2002 Ada Core Technologies, Inc.           --
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

with Unchecked_Conversion;

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
      function To_Char is new Unchecked_Conversion
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
      function To_S2 is new Unchecked_Conversion (Wide_Character, S2);

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

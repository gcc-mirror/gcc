------------------------------------------------------------------------------
--                                                                          --
--                          GNAT RUN-TIME COMPONENTS                        --
--                                                                          --
--            A D A . S T R I N G S . U N B O U N D E D . A U X             --
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

package body Ada.Strings.Unbounded.Aux is

   ----------------
   -- Get_String --
   ----------------

   procedure Get_String
     (U : Unbounded_String;
      S : out Big_String_Access;
      L : out Natural)
   is
      X : aliased Big_String;
      for X'Address use U.Reference.Data'Address;
   begin
      S := X'Unchecked_Access;
      L := U.Reference.Last;
   end Get_String;

   ----------------
   -- Set_String --
   ----------------

   procedure Set_String
     (U      : out Unbounded_String;
      Length : Positive;
      Set    : not null access procedure (S : out String))
   is
      TR : constant Shared_String_Access := U.Reference;
      DR : Shared_String_Access;
   begin
      --  Try to reuse existing shared string

      if Can_Be_Reused (TR, Length) then
         Reference (TR);
         DR := TR;

      --  Otherwise allocate new shared string

      else
         DR := Allocate (Length);
         U.Reference := DR;
      end if;

      Set (DR.Data (1 .. Length));
      DR.Last := Length;
      Unreference (TR);
   end Set_String;

end Ada.Strings.Unbounded.Aux;

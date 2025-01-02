------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . I M A G E _ N                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 2021-2025, Free Software Foundation, Inc.      --
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

package body System.Image_N is

   -----------------------
   -- Image_Enumeration --
   -----------------------

   procedure Image_Enumeration
     (Pos     : Natural;
      S       : in out String;
      P       : out Natural;
      Names   : String;
      Indexes : System.Address)
   is
      pragma Assert (S'First = 1);

      subtype Names_Index is
        Index_Type range Index_Type (Names'First)
                          .. Index_Type (Names'Last) + 1;
      subtype Index is Natural range Natural'First .. Names'Length;
      type Index_Table is array (Index) of Names_Index;
      type Index_Table_Ptr is access Index_Table;

      function To_Index_Table_Ptr is
        new Ada.Unchecked_Conversion (System.Address, Index_Table_Ptr);

      IndexesT : constant Index_Table_Ptr := To_Index_Table_Ptr (Indexes);

      pragma Assert (Pos in IndexesT'Range);
      pragma Assert (Pos + 1 in IndexesT'Range);

      Start : constant Natural := Natural (IndexesT (Pos));
      Next  : constant Natural := Natural (IndexesT (Pos + 1));

      pragma Assert (Next - 1 >= Start);
      pragma Assert (Start >= Names'First);
      pragma Assert (Next - 1 <= Names'Last);

      pragma Assert (Next - Start <= S'Last);
      --  The caller should guarantee that S is large enough to contain the
      --  enumeration image.
   begin
      S (1 .. Next - Start) := Names (Start .. Next - 1);
      P := Next - Start;
   end Image_Enumeration;

end System.Image_N;

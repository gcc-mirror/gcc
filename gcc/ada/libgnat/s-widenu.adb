------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                      S Y S T E M . W I D _ E N U M                       --
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

with Ada.Unchecked_Conversion;

package body System.Wid_Enum is

   -------------------------
   -- Width_Enumeration_8 --
   -------------------------

   function Width_Enumeration_8
     (Names   : String;
      Indexes : System.Address;
      Lo, Hi  : Natural)
      return    Natural
   is
      pragma Warnings (Off, Names);

      W : Natural;

      type Natural_8 is range 0 .. 2 ** 7 - 1;
      type Index_Table is array (Natural) of Natural_8;
      type Index_Table_Ptr is access Index_Table;

      function To_Index_Table_Ptr is
        new Ada.Unchecked_Conversion (System.Address, Index_Table_Ptr);

      IndexesT : constant Index_Table_Ptr := To_Index_Table_Ptr (Indexes);

   begin
      W := 0;

      for J in Lo .. Hi loop
         W := Natural'Max (W, Natural (IndexesT (J + 1) - IndexesT (J)));
      end loop;

      return W;
   end Width_Enumeration_8;

   --------------------------
   -- Width_Enumeration_16 --
   --------------------------

   function Width_Enumeration_16
     (Names   : String;
      Indexes : System.Address;
      Lo, Hi  : Natural)
      return    Natural
   is
      pragma Warnings (Off, Names);

      W : Natural;

      type Natural_16 is range 0 .. 2 ** 15 - 1;
      type Index_Table is array (Natural) of Natural_16;
      type Index_Table_Ptr is access Index_Table;

      function To_Index_Table_Ptr is
        new Ada.Unchecked_Conversion (System.Address, Index_Table_Ptr);

      IndexesT : constant Index_Table_Ptr := To_Index_Table_Ptr (Indexes);

   begin
      W := 0;

      for J in Lo .. Hi loop
         W := Natural'Max (W, Natural (IndexesT (J + 1) - IndexesT (J)));
      end loop;

      return W;
   end Width_Enumeration_16;

   --------------------------
   -- Width_Enumeration_32 --
   --------------------------

   function Width_Enumeration_32
     (Names   : String;
      Indexes : System.Address;
      Lo, Hi  : Natural)
      return    Natural
   is
      pragma Warnings (Off, Names);

      W : Natural;

      type Natural_32 is range 0 .. 2 ** 31 - 1;
      type Index_Table is array (Natural) of Natural_32;
      type Index_Table_Ptr is access Index_Table;

      function To_Index_Table_Ptr is
        new Ada.Unchecked_Conversion (System.Address, Index_Table_Ptr);

      IndexesT : constant Index_Table_Ptr := To_Index_Table_Ptr (Indexes);

   begin
      W := 0;

      for J in Lo .. Hi loop
         W := Natural'Max (W, Natural (IndexesT (J + 1) - IndexesT (J)));
      end loop;

      return W;
   end Width_Enumeration_32;

end System.Wid_Enum;

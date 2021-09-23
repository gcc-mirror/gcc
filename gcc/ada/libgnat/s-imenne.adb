------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                  S Y S T E M . I M G _ E N U M _ N E W                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2000-2021, Free Software Foundation, Inc.         --
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

pragma Compiler_Unit_Warning;

with Ada.Unchecked_Conversion;

package body System.Img_Enum_New is

   -------------------------
   -- Image_Enumeration_8 --
   -------------------------

   procedure Image_Enumeration_8
     (Pos     : Natural;
      S       : in out String;
      P       : out Natural;
      Names   : String;
      Indexes : System.Address)
   is
      pragma Assert (S'First = 1);

      type Natural_8 is range 0 .. 2 ** 7 - 1;
      subtype Names_Index is
        Natural_8 range Natural_8 (Names'First)
                          .. Natural_8 (Names'Last) + 1;
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
   end Image_Enumeration_8;

   --------------------------
   -- Image_Enumeration_16 --
   --------------------------

   procedure Image_Enumeration_16
     (Pos     : Natural;
      S       : in out String;
      P       : out Natural;
      Names   : String;
      Indexes : System.Address)
   is
      pragma Assert (S'First = 1);

      type Natural_16 is range 0 .. 2 ** 15 - 1;
      subtype Names_Index is
        Natural_16 range Natural_16 (Names'First)
                           .. Natural_16 (Names'Last) + 1;
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
   end Image_Enumeration_16;

   --------------------------
   -- Image_Enumeration_32 --
   --------------------------

   procedure Image_Enumeration_32
     (Pos     : Natural;
      S       : in out String;
      P       : out Natural;
      Names   : String;
      Indexes : System.Address)
   is
      pragma Assert (S'First = 1);

      type Natural_32 is range 0 .. 2 ** 31 - 1;
      subtype Names_Index is
        Natural_32 range Natural_32 (Names'First)
                           .. Natural_32 (Names'Last) + 1;
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
   end Image_Enumeration_32;

end System.Img_Enum_New;

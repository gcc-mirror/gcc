------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--         A D A . W I D E _ C H A R A C T E R T S . U N I C O D E          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2005-2020, Free Software Foundation, Inc.         --
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

package body Ada.Wide_Characters.Unicode is

   package G renames System.UTF_32;

   ------------------
   -- Get_Category --
   ------------------

   function Get_Category (U : Wide_Character) return Category is
   begin
      return Category (G.Get_Category (Wide_Character'Pos (U)));
   end Get_Category;

   --------------
   -- Is_Basic --
   --------------

   function Is_Basic (U : Wide_Character) return Boolean is
   begin
      return G.Is_UTF_32_Basic (Wide_Character'Pos (U));
   end Is_Basic;

   --------------
   -- Is_Digit --
   --------------

   function Is_Digit (U : Wide_Character) return Boolean is
   begin
      return G.Is_UTF_32_Digit (Wide_Character'Pos (U));
   end Is_Digit;

   function Is_Digit (C : Category) return Boolean is
   begin
      return G.Is_UTF_32_Digit (G.Category (C));
   end Is_Digit;

   ---------------
   -- Is_Letter --
   ---------------

   function Is_Letter (U : Wide_Character) return Boolean is
   begin
      return G.Is_UTF_32_Letter (Wide_Character'Pos (U));
   end Is_Letter;

   function Is_Letter (C : Category) return Boolean is
   begin
      return G.Is_UTF_32_Letter (G.Category (C));
   end Is_Letter;

   ------------------------
   -- Is_Line_Terminator --
   ------------------------

   function Is_Line_Terminator (U : Wide_Character) return Boolean is
   begin
      return G.Is_UTF_32_Line_Terminator (Wide_Character'Pos (U));
   end Is_Line_Terminator;

   -------------
   -- Is_Mark --
   -------------

   function Is_Mark (U : Wide_Character) return Boolean is
   begin
      return G.Is_UTF_32_Mark (Wide_Character'Pos (U));
   end Is_Mark;

   function Is_Mark (C : Category) return Boolean is
   begin
      return G.Is_UTF_32_Mark (G.Category (C));
   end Is_Mark;

   --------------------
   -- Is_Non_Graphic --
   --------------------

   function Is_Non_Graphic (U : Wide_Character) return Boolean is
   begin
      return G.Is_UTF_32_Non_Graphic (Wide_Character'Pos (U));
   end Is_Non_Graphic;

   function Is_Non_Graphic (C : Category) return Boolean is
   begin
      return G.Is_UTF_32_Non_Graphic (G.Category (C));
   end Is_Non_Graphic;

   -------------
   -- Is_NFKC --
   -------------

   function Is_NFKC (U : Wide_Character) return Boolean is
   begin
      return G.Is_UTF_32_NFKC (Wide_Character'Pos (U));
   end Is_NFKC;

   --------------
   -- Is_Other --
   --------------

   function Is_Other (U : Wide_Character) return Boolean is
   begin
      return G.Is_UTF_32_Other (Wide_Character'Pos (U));
   end Is_Other;

   function Is_Other (C : Category) return Boolean is
   begin
      return G.Is_UTF_32_Other (G.Category (C));
   end Is_Other;

   --------------------
   -- Is_Punctuation --
   --------------------

   function Is_Punctuation (U : Wide_Character) return Boolean is
   begin
      return G.Is_UTF_32_Punctuation (Wide_Character'Pos (U));
   end Is_Punctuation;

   function Is_Punctuation (C : Category) return Boolean is
   begin
      return G.Is_UTF_32_Punctuation (G.Category (C));
   end Is_Punctuation;

   --------------
   -- Is_Space --
   --------------

   function Is_Space (U : Wide_Character) return Boolean is
   begin
      return G.Is_UTF_32_Space (Wide_Character'Pos (U));
   end Is_Space;

   function Is_Space (C : Category) return Boolean is
   begin
      return G.Is_UTF_32_Space (G.Category (C));
   end Is_Space;

   --------------
   -- To_Basic --
   --------------

   function To_Basic (U : Wide_Character) return Wide_Character is
   begin
      return Wide_Character'Val (G.UTF_32_To_Basic (Wide_Character'Pos (U)));
   end To_Basic;

   -------------------
   -- To_Lower_Case --
   -------------------

   function To_Lower_Case (U : Wide_Character) return Wide_Character is
   begin
      return
        Wide_Character'Val (G.UTF_32_To_Lower_Case (Wide_Character'Pos (U)));
   end To_Lower_Case;

   -------------------
   -- To_Upper_Case --
   -------------------

   function To_Upper_Case (U : Wide_Character) return Wide_Character is
   begin
      return
        Wide_Character'Val
          (G.UTF_32_To_Upper_Case (Wide_Character'Pos (U)));
   end To_Upper_Case;

end Ada.Wide_Characters.Unicode;

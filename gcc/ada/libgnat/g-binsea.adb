------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                           GNAT.BINARY_SEARCH                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                        Copyright (C) 2022-2023, AdaCore                  --
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
------------------------------------------------------------------------------

package body GNAT.Binary_Search is

   function Index
     (First, Last, Start : Index_Type;
      Element            : Element_Type) return Index_Type'Base is
   begin
      if Leftmost then
         declare
            function Before
              (Index : Index_Type; Element : Element_Type) return Boolean
            is (Before (Get (Index), Element)) with Inline_Always;

            function Find is new Binary_Search.Leftmost
              (Index_Type, Element_Type, Before);
         begin
            return Find (First, Last, Start, Element);
         end;

      else
         declare
            function Before
              (Element : Element_Type; Index : Index_Type) return Boolean
            is (Before (Element, Get (Index))) with Inline_Always;

            function Find is new Rightmost (Index_Type, Element_Type, Before);
         begin
            return Find (First, Last, Start, Element);
         end;
      end if;
   end Index;

   --------------
   -- Leftmost --
   --------------

   function Leftmost
     (First, Last, Start : Index_Type;
      Element            : Element_Type) return Index_Type'Base
   is
      L : Index_Type := First;
      R : Index_Type := Index_Type'Succ (Last);
      M : Index_Type := Start;
   begin
      if First <= Last then
         loop
            if Before (M, Element) then
               L := Index_Type'Succ (M);
            else
               R := M;
            end if;

            exit when L >= R;

            M := Index_Type'Val
              (Index_Type'Pos (L) +
               (Index_Type'Pos (R) - Index_Type'Pos (L)) / 2);
         end loop;
      end if;

      return L;
   end Leftmost;

   ---------------
   -- Rightmost --
   ---------------

   function Rightmost
     (First, Last, Start : Index_Type;
      Element            : Element_Type) return Index_Type'Base
   is
      L : Index_Type := First;
      R : Index_Type := Index_Type'Succ (Last);
      M : Index_Type := Start;
   begin
      if First > Last then
         return Last;
      else
         loop
            if Before (Element, M) then
               R := M;
            else
               L := Index_Type'Succ (M);
            end if;

            exit when L >= R;

            M := Index_Type'Val
              (Index_Type'Pos (L) +
               (Index_Type'Pos (R) - Index_Type'Pos (L)) / 2);
         end loop;
      end if;

      return Index_Type'Pred (R);
   end Rightmost;

end GNAT.Binary_Search;

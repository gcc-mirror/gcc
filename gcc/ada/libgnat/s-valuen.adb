------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . V A L U E _ N                        --
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

with System.Val_Util; use System.Val_Util;

package body System.Value_N is

   function Value_Enumeration_Pos
     (Names   : String;
      Indexes : System.Address;
      Hash    : Hash_Function_Ptr;
      Num     : Natural;
      Is_Wide : Boolean;
      Str     : String)
      return    Integer with Pure_Function;
   --  Same as Value_Enumeration, except returns negative if Value_Enumeration
   --  would raise Constraint_Error.

   ---------------------------
   -- Value_Enumeration_Pos --
   ---------------------------

   function Value_Enumeration_Pos
     (Names   : String;
      Indexes : System.Address;
      Hash    : Hash_Function_Ptr;
      Num     : Natural;
      Is_Wide : Boolean;
      Str     : String)
      return    Integer
   is
      F, L : Integer;
      H  : Natural;
      S  : String (Str'Range) := Str;

      subtype Names_Index is
        Index_Type range Index_Type (Names'First)
                          .. Index_Type (Names'Last) + 1;
      subtype Index is Natural range Natural'First .. Names'Length;
      type Index_Table is array (Index) of Names_Index;
      type Index_Table_Ptr is access Index_Table;

      function To_Index_Table_Ptr is
        new Ada.Unchecked_Conversion (System.Address, Index_Table_Ptr);

      IndexesT : constant Index_Table_Ptr := To_Index_Table_Ptr (Indexes);

      pragma Assert (Num + 1 in IndexesT'Range);

   begin
      Normalize_String (S, F, L, To_Upper_Case => not Is_Wide);

      declare
         Normal : String renames S (F .. L);

      begin
         --  If we have a valid hash value, do a single lookup

         H := (if Hash /= null then Hash.all (Normal) else Natural'Last);

         if H /= Natural'Last then
            if Names
              (Natural (IndexesT (H)) ..
               Natural (IndexesT (H + 1)) - 1) = Normal
            then
               return H;
            end if;

         --  Otherwise do a linear search

         else
            for J in 0 .. Num loop
               if Names
                 (Natural (IndexesT (J)) ..
                  Natural (IndexesT (J + 1)) - 1) = Normal
               then
                  return J;
               end if;
            end loop;
         end if;
      end;

      return -1;
   end Value_Enumeration_Pos;

   -----------------------------
   -- Valid_Value_Enumeration --
   -----------------------------

   function Valid_Value_Enumeration
     (Names   : String;
      Indexes : System.Address;
      Hash    : Hash_Function_Ptr;
      Num     : Natural;
      Is_Wide : Boolean;
      Str     : String)
      return    Boolean
   is
   begin
      return
        Value_Enumeration_Pos (Names, Indexes, Hash, Num, Is_Wide, Str) >= 0;
   end Valid_Value_Enumeration;

   -----------------------
   -- Value_Enumeration --
   -----------------------

   function Value_Enumeration
     (Names   : String;
      Indexes : System.Address;
      Hash    : Hash_Function_Ptr;
      Num     : Natural;
      Is_Wide : Boolean;
      Str     : String)
      return    Natural
   is
      Result : constant Integer :=
        Value_Enumeration_Pos (Names, Indexes, Hash, Num, Is_Wide, Str);

   begin
      --  The comparison eliminates the need for a range check on return

      if Result < 0 then
         Bad_Value (Str);
      else
         return Result;
      end if;
   end Value_Enumeration;

end System.Value_N;

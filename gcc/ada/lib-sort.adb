------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             L I B . S O R T                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--   Copyright (C) 1992-2002 Free Software Foundation, Inc.                 --
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

with GNAT.Heap_Sort_A; use GNAT.Heap_Sort_A;

separate (Lib)
procedure Sort (Tbl : in out Unit_Ref_Table) is

   T : array (0 .. Integer (Tbl'Last - Tbl'First + 1)) of Unit_Number_Type;
   --  Actual sort is done on this copy of the array with 0's origin
   --  subscripts. Location 0 is used as a temporary by the sorting algorithm.
   --  Also the addressing of the table is more efficient with 0's origin,
   --  even though we have to copy Tbl back and forth.

   function Lt_Uname (C1, C2 : Natural) return Boolean;
   --  Comparison routine for comparing Unames. Needed by the sorting routine.

   procedure Move_Uname (From : Natural; To : Natural);
   --  Move routine needed by the sorting routine below.

   --------------
   -- Lt_Uname --
   --------------

   function Lt_Uname (C1, C2 : Natural) return Boolean is
   begin
      --  Preprocessing data and definition files are not sorted, they are
      --  at the bottom of the list. They are recognized because they are
      --  the only ones without a Unit_Name.

      if Units.Table (T (C1)).Unit_Name = No_Name then
         return False;

      elsif Units.Table (T (C2)).Unit_Name = No_Name then
         return True;

      else
         return
           Uname_Lt
             (Units.Table (T (C1)).Unit_Name, Units.Table (T (C2)).Unit_Name);
      end if;
   end Lt_Uname;

   ----------------
   -- Move_Uname --
   ----------------

   procedure Move_Uname (From : Natural; To : Natural) is
   begin
      T (To) := T (From);
   end Move_Uname;

--  Start of processing for Sort

begin
   if T'Last > 0 then
      for I in 1 .. T'Last loop
         T (I) := Tbl (Int (I) - 1 + Tbl'First);
      end loop;

      Sort (T'Last,
        Move_Uname'Unrestricted_Access, Lt_Uname'Unrestricted_Access);

   --  Sort is complete, copy result back into place

      for I in 1 .. T'Last loop
         Tbl (Int (I) - 1 + Tbl'First) := T (I);
      end loop;
   end if;
end Sort;

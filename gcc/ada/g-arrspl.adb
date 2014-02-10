------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     G N A T . A R R A Y _ S P L I T                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2002-2013, Free Software Foundation, Inc.         --
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

with Ada.Unchecked_Deallocation;

package body GNAT.Array_Split is

   procedure Free is
      new Ada.Unchecked_Deallocation (Slices_Indexes, Slices_Access);

   procedure Free is
      new Ada.Unchecked_Deallocation (Separators_Indexes, Indexes_Access);

   function Count
     (Source  : Element_Sequence;
      Pattern : Element_Set) return Natural;
   --  Returns the number of occurrences of Pattern elements in Source, 0 is
   --  returned if no occurrence is found in Source.

   ------------
   -- Adjust --
   ------------

   procedure Adjust (S : in out Slice_Set) is
   begin
      S.D.Ref_Counter := S.D.Ref_Counter + 1;
   end Adjust;

   ------------
   -- Create --
   ------------

   procedure Create
     (S          : out Slice_Set;
      From       : Element_Sequence;
      Separators : Element_Sequence;
      Mode       : Separator_Mode := Single)
   is
   begin
      Create (S, From, To_Set (Separators), Mode);
   end Create;

   ------------
   -- Create --
   ------------

   procedure Create
     (S          : out Slice_Set;
      From       : Element_Sequence;
      Separators : Element_Set;
      Mode       : Separator_Mode := Single)
   is
      Result : Slice_Set;
   begin
      Result.D.Source := new Element_Sequence'(From);
      Set (Result, Separators, Mode);
      S := Result;
   end Create;

   -----------
   -- Count --
   -----------

   function Count
     (Source  : Element_Sequence;
      Pattern : Element_Set) return Natural
   is
      C : Natural := 0;
   begin
      for K in Source'Range loop
         if Is_In (Source (K), Pattern) then
            C := C + 1;
         end if;
      end loop;

      return C;
   end Count;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (S : in out Slice_Set) is

      procedure Free is
         new Ada.Unchecked_Deallocation (Element_Sequence, Element_Access);

      procedure Free is
         new Ada.Unchecked_Deallocation (Data, Data_Access);

      D : Data_Access := S.D;

   begin
      --  Ensure call is idempotent

      S.D := null;

      if D /= null then
         D.Ref_Counter := D.Ref_Counter - 1;

         if D.Ref_Counter = 0 then
            Free (D.Source);
            Free (D.Indexes);
            Free (D.Slices);
            Free (D);
         end if;
      end if;
   end Finalize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (S : in out Slice_Set) is
   begin
      S.D := new Data'(1, null, 0, null, null);
   end Initialize;

   ----------------
   -- Separators --
   ----------------

   function Separators
     (S     : Slice_Set;
      Index : Slice_Number) return Slice_Separators
   is
   begin
      if Index > S.D.N_Slice then
         raise Index_Error;

      elsif Index = 0
        or else (Index = 1 and then S.D.N_Slice = 1)
      then
         --  Whole string, or no separator used

         return (Before => Array_End,
                 After  => Array_End);

      elsif Index = 1 then
         return (Before => Array_End,
                 After  => S.D.Source (S.D.Slices (Index).Stop + 1));

      elsif Index = S.D.N_Slice then
         return (Before => S.D.Source (S.D.Slices (Index).Start - 1),
                 After  => Array_End);

      else
         return (Before => S.D.Source (S.D.Slices (Index).Start - 1),
                 After  => S.D.Source (S.D.Slices (Index).Stop + 1));
      end if;
   end Separators;

   ----------------
   -- Separators --
   ----------------

   function Separators (S : Slice_Set) return Separators_Indexes is
   begin
      return S.D.Indexes.all;
   end Separators;

   ---------
   -- Set --
   ---------

   procedure Set
     (S          : in out Slice_Set;
      Separators : Element_Sequence;
      Mode       : Separator_Mode := Single)
   is
   begin
      Set (S, To_Set (Separators), Mode);
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (S          : in out Slice_Set;
      Separators : Element_Set;
      Mode       : Separator_Mode := Single)
   is

      procedure Copy_On_Write (S : in out Slice_Set);
      --  Make a copy of S if shared with another variable

      -------------------
      -- Copy_On_Write --
      -------------------

      procedure Copy_On_Write (S : in out Slice_Set) is
      begin
         if S.D.Ref_Counter > 1 then
            --  First let's remove our count from the current data

            S.D.Ref_Counter := S.D.Ref_Counter - 1;

            --  Then duplicate the data

            S.D := new Data'(S.D.all);
            S.D.Ref_Counter := 1;

            if S.D.Source /= null then
               S.D.Source := new Element_Sequence'(S.D.Source.all);
               S.D.Indexes := null;
               S.D.Slices := null;
            end if;

         else
            --  If there is a single reference to this variable, free it now
            --  as it will be redefined below.

            Free (S.D.Indexes);
            Free (S.D.Slices);
         end if;
      end Copy_On_Write;

      Count_Sep : constant Natural := Count (S.D.Source.all, Separators);
      J         : Positive;

   begin
      Copy_On_Write (S);

      --  Compute all separator's indexes

      S.D.Indexes := new Separators_Indexes (1 .. Count_Sep);
      J := S.D.Indexes'First;

      for K in S.D.Source'Range loop
         if Is_In (S.D.Source (K), Separators) then
            S.D.Indexes (J) := K;
            J := J + 1;
         end if;
      end loop;

      --  Compute slice info for fast slice access

      declare
         S_Info      : Slices_Indexes (1 .. Slice_Number (Count_Sep) + 1);
         K           : Natural := 1;
         Start, Stop : Natural;

      begin
         S.D.N_Slice := 0;

         Start := S.D.Source'First;
         Stop  := 0;

         loop
            if K > Count_Sep then

               --  No more separators, last slice ends at end of source string

               Stop := S.D.Source'Last;

            else
               Stop := S.D.Indexes (K) - 1;
            end if;

            --  Add slice to the table

            S.D.N_Slice := S.D.N_Slice + 1;
            S_Info (S.D.N_Slice) := (Start, Stop);

            exit when K > Count_Sep;

            case Mode is

               when Single =>

                  --  In this mode just set start to character next to the
                  --  current separator, advance the separator index.

                  Start := S.D.Indexes (K) + 1;
                  K := K + 1;

               when Multiple =>

                  --  In this mode skip separators following each other

                  loop
                     Start := S.D.Indexes (K) + 1;
                     K := K + 1;
                     exit when K > Count_Sep
                       or else S.D.Indexes (K) > S.D.Indexes (K - 1) + 1;
                  end loop;

            end case;
         end loop;

         S.D.Slices := new Slices_Indexes'(S_Info (1 .. S.D.N_Slice));
      end;
   end Set;

   -----------
   -- Slice --
   -----------

   function Slice
     (S     : Slice_Set;
      Index : Slice_Number) return Element_Sequence
   is
   begin
      if Index = 0 then
         return S.D.Source.all;

      elsif Index > S.D.N_Slice then
         raise Index_Error;

      else
         return
           S.D.Source (S.D.Slices (Index).Start .. S.D.Slices (Index).Stop);
      end if;
   end Slice;

   -----------------
   -- Slice_Count --
   -----------------

   function Slice_Count (S : Slice_Set) return Slice_Number is
   begin
      return S.D.N_Slice;
   end Slice_Count;

end GNAT.Array_Split;

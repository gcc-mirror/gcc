------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     G N A T . A R R A Y _ S P L I T                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2002-2009, Free Software Foundation, Inc.         --
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

   procedure Free is
      new Ada.Unchecked_Deallocation (Element_Sequence, Element_Access);

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
      S.Ref_Counter.all := S.Ref_Counter.all + 1;
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
   begin
      Free (S.Source);
      S.Source := new Element_Sequence'(From);
      Set (S, Separators, Mode);
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
         new Ada.Unchecked_Deallocation (Natural, Counter);

   begin
      S.Ref_Counter.all := S.Ref_Counter.all - 1;

      if S.Ref_Counter.all = 0 then
         Free (S.Source);
         Free (S.Indexes);
         Free (S.Slices);
         Free (S.Ref_Counter);
      end if;
   end Finalize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (S : in out Slice_Set) is
   begin
      S.Ref_Counter := new Natural'(1);
   end Initialize;

   ----------------
   -- Separators --
   ----------------

   function Separators
     (S     : Slice_Set;
      Index : Slice_Number) return Slice_Separators
   is
   begin
      if Index > S.N_Slice then
         raise Index_Error;

      elsif Index = 0
        or else (Index = 1 and then S.N_Slice = 1)
      then
         --  Whole string, or no separator used

         return (Before => Array_End,
                 After  => Array_End);

      elsif Index = 1 then
         return (Before => Array_End,
                 After  => S.Source (S.Slices (Index).Stop + 1));

      elsif Index = S.N_Slice then
         return (Before => S.Source (S.Slices (Index).Start - 1),
                 After  => Array_End);

      else
         return (Before => S.Source (S.Slices (Index).Start - 1),
                 After  => S.Source (S.Slices (Index).Stop + 1));
      end if;
   end Separators;

   ----------------
   -- Separators --
   ----------------

   function Separators (S : Slice_Set) return Separators_Indexes is
   begin
      return S.Indexes.all;
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
      Count_Sep : constant Natural := Count (S.Source.all, Separators);
      J : Positive;
   begin
      --  Free old structure
      Free (S.Indexes);
      Free (S.Slices);

      --  Compute all separator's indexes

      S.Indexes := new Separators_Indexes (1 .. Count_Sep);
      J := S.Indexes'First;

      for K in S.Source'Range loop
         if Is_In (S.Source (K), Separators) then
            S.Indexes (J) := K;
            J := J + 1;
         end if;
      end loop;

      --  Compute slice info for fast slice access

      declare
         S_Info      : Slices_Indexes (1 .. Slice_Number (Count_Sep) + 1);
         K           : Natural := 1;
         Start, Stop : Natural;

      begin
         S.N_Slice := 0;

         Start := S.Source'First;
         Stop  := 0;

         loop
            if K > Count_Sep then

               --  No more separators, last slice ends at the end of the source
               --  string.

               Stop := S.Source'Last;
            else
               Stop := S.Indexes (K) - 1;
            end if;

            --  Add slice to the table

            S.N_Slice := S.N_Slice + 1;
            S_Info (S.N_Slice) := (Start, Stop);

            exit when K > Count_Sep;

            case Mode is

               when Single =>

                  --  In this mode just set start to character next to the
                  --  current separator, advance the separator index.

                  Start := S.Indexes (K) + 1;
                  K := K + 1;

               when Multiple =>

                  --  In this mode skip separators following each other

                  loop
                     Start := S.Indexes (K) + 1;
                     K := K + 1;
                     exit when K > Count_Sep
                       or else S.Indexes (K) > S.Indexes (K - 1) + 1;
                  end loop;

            end case;
         end loop;

         S.Slices := new Slices_Indexes'(S_Info (1 .. S.N_Slice));
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
         return S.Source.all;

      elsif Index > S.N_Slice then
         raise Index_Error;

      else
         return S.Source (S.Slices (Index).Start .. S.Slices (Index).Stop);
      end if;
   end Slice;

   -----------------
   -- Slice_Count --
   -----------------

   function Slice_Count (S : Slice_Set) return Slice_Number is
   begin
      return S.N_Slice;
   end Slice_Count;

end GNAT.Array_Split;

------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--           A D A . S T R I N G S . W I D E _ U N B O U N D E D            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2012, Free Software Foundation, Inc.         --
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

with Ada.Strings.Wide_Search;
with Ada.Unchecked_Deallocation;

package body Ada.Strings.Wide_Unbounded is

   use Ada.Strings.Wide_Maps;

   Growth_Factor : constant := 32;
   --  The growth factor controls how much extra space is allocated when
   --  we have to increase the size of an allocated unbounded string. By
   --  allocating extra space, we avoid the need to reallocate on every
   --  append, particularly important when a string is built up by repeated
   --  append operations of small pieces. This is expressed as a factor so
   --  32 means add 1/32 of the length of the string as growth space.

   Min_Mul_Alloc : constant := Standard'Maximum_Alignment;
   --  Allocation will be done by a multiple of Min_Mul_Alloc. This causes
   --  no memory loss as most (all?) malloc implementations are obliged to
   --  align the returned memory on the maximum alignment as malloc does not
   --  know the target alignment.

   function Aligned_Max_Length (Max_Length : Natural) return Natural;
   --  Returns recommended length of the shared string which is greater or
   --  equal to specified length. Calculation take in sense alignment of
   --  the allocated memory segments to use memory effectively by
   --  Append/Insert/etc operations.

   ---------
   -- "&" --
   ---------

   function "&"
     (Left  : Unbounded_Wide_String;
      Right : Unbounded_Wide_String) return Unbounded_Wide_String
   is
      LR : constant Shared_Wide_String_Access := Left.Reference;
      RR : constant Shared_Wide_String_Access := Right.Reference;
      DL : constant Natural := LR.Last + RR.Last;
      DR : Shared_Wide_String_Access;

   begin
      --  Result is an empty string, reuse shared empty string

      if DL = 0 then
         Reference (Empty_Shared_Wide_String'Access);
         DR := Empty_Shared_Wide_String'Access;

      --  Left string is empty, return Rigth string

      elsif LR.Last = 0 then
         Reference (RR);
         DR := RR;

      --  Right string is empty, return Left string

      elsif RR.Last = 0 then
         Reference (LR);
         DR := LR;

      --  Overwise, allocate new shared string and fill data

      else
         DR := Allocate (LR.Last + RR.Last);
         DR.Data (1 .. LR.Last) := LR.Data (1 .. LR.Last);
         DR.Data (LR.Last + 1 .. DL) := RR.Data (1 .. RR.Last);
         DR.Last := DL;
      end if;

      return (AF.Controlled with Reference => DR);
   end "&";

   function "&"
     (Left  : Unbounded_Wide_String;
      Right : Wide_String) return Unbounded_Wide_String
   is
      LR : constant Shared_Wide_String_Access := Left.Reference;
      DL : constant Natural := LR.Last + Right'Length;
      DR : Shared_Wide_String_Access;

   begin
      --  Result is an empty string, reuse shared empty string

      if DL = 0 then
         Reference (Empty_Shared_Wide_String'Access);
         DR := Empty_Shared_Wide_String'Access;

      --  Right is an empty string, return Left string

      elsif Right'Length = 0 then
         Reference (LR);
         DR := LR;

      --  Otherwise, allocate new shared string and fill it

      else
         DR := Allocate (DL);
         DR.Data (1 .. LR.Last) := LR.Data (1 .. LR.Last);
         DR.Data (LR.Last + 1 .. DL) := Right;
         DR.Last := DL;
      end if;

      return (AF.Controlled with Reference => DR);
   end "&";

   function "&"
     (Left  : Wide_String;
      Right : Unbounded_Wide_String) return Unbounded_Wide_String
   is
      RR : constant Shared_Wide_String_Access := Right.Reference;
      DL : constant Natural := Left'Length + RR.Last;
      DR : Shared_Wide_String_Access;

   begin
      --  Result is an empty string, reuse shared one

      if DL = 0 then
         Reference (Empty_Shared_Wide_String'Access);
         DR := Empty_Shared_Wide_String'Access;

      --  Left is empty string, return Right string

      elsif Left'Length = 0 then
         Reference (RR);
         DR := RR;

      --  Otherwise, allocate new shared string and fill it

      else
         DR := Allocate (DL);
         DR.Data (1 .. Left'Length) := Left;
         DR.Data (Left'Length + 1 .. DL) := RR.Data (1 .. RR.Last);
         DR.Last := DL;
      end if;

      return (AF.Controlled with Reference => DR);
   end "&";

   function "&"
     (Left  : Unbounded_Wide_String;
      Right : Wide_Character) return Unbounded_Wide_String
   is
      LR : constant Shared_Wide_String_Access := Left.Reference;
      DL : constant Natural := LR.Last + 1;
      DR : Shared_Wide_String_Access;

   begin
      DR := Allocate (DL);
      DR.Data (1 .. LR.Last) := LR.Data (1 .. LR.Last);
      DR.Data (DL) := Right;
      DR.Last := DL;

      return (AF.Controlled with Reference => DR);
   end "&";

   function "&"
     (Left  : Wide_Character;
      Right : Unbounded_Wide_String) return Unbounded_Wide_String
   is
      RR : constant Shared_Wide_String_Access := Right.Reference;
      DL : constant Natural := 1 + RR.Last;
      DR : Shared_Wide_String_Access;

   begin
      DR := Allocate (DL);
      DR.Data (1) := Left;
      DR.Data (2 .. DL) := RR.Data (1 .. RR.Last);
      DR.Last := DL;

      return (AF.Controlled with Reference => DR);
   end "&";

   ---------
   -- "*" --
   ---------

   function "*"
     (Left  : Natural;
      Right : Wide_Character) return Unbounded_Wide_String
   is
      DR : Shared_Wide_String_Access;

   begin
      --  Result is an empty string, reuse shared empty string

      if Left = 0 then
         Reference (Empty_Shared_Wide_String'Access);
         DR := Empty_Shared_Wide_String'Access;

      --  Otherwise, allocate new shared string and fill it

      else
         DR := Allocate (Left);

         for J in 1 .. Left loop
            DR.Data (J) := Right;
         end loop;

         DR.Last := Left;
      end if;

      return (AF.Controlled with Reference => DR);
   end "*";

   function "*"
     (Left  : Natural;
      Right : Wide_String) return Unbounded_Wide_String
   is
      DL : constant Natural := Left * Right'Length;
      DR : Shared_Wide_String_Access;
      K  : Positive;

   begin
      --  Result is an empty string, reuse shared empty string

      if DL = 0 then
         Reference (Empty_Shared_Wide_String'Access);
         DR := Empty_Shared_Wide_String'Access;

      --  Otherwise, allocate new shared string and fill it

      else
         DR := Allocate (DL);
         K := 1;

         for J in 1 .. Left loop
            DR.Data (K .. K + Right'Length - 1) := Right;
            K := K + Right'Length;
         end loop;

         DR.Last := DL;
      end if;

      return (AF.Controlled with Reference => DR);
   end "*";

   function "*"
     (Left  : Natural;
      Right : Unbounded_Wide_String) return Unbounded_Wide_String
   is
      RR : constant Shared_Wide_String_Access := Right.Reference;
      DL : constant Natural := Left * RR.Last;
      DR : Shared_Wide_String_Access;
      K  : Positive;

   begin
      --  Result is an empty string, reuse shared empty string

      if DL = 0 then
         Reference (Empty_Shared_Wide_String'Access);
         DR := Empty_Shared_Wide_String'Access;

      --  Coefficient is one, just return string itself

      elsif Left = 1 then
         Reference (RR);
         DR := RR;

      --  Otherwise, allocate new shared string and fill it

      else
         DR := Allocate (DL);
         K := 1;

         for J in 1 .. Left loop
            DR.Data (K .. K + RR.Last - 1) := RR.Data (1 .. RR.Last);
            K := K + RR.Last;
         end loop;

         DR.Last := DL;
      end if;

      return (AF.Controlled with Reference => DR);
   end "*";

   ---------
   -- "<" --
   ---------

   function "<"
     (Left  : Unbounded_Wide_String;
      Right : Unbounded_Wide_String) return Boolean
   is
      LR : constant Shared_Wide_String_Access := Left.Reference;
      RR : constant Shared_Wide_String_Access := Right.Reference;
   begin
      return LR.Data (1 .. LR.Last) < RR.Data (1 .. RR.Last);
   end "<";

   function "<"
     (Left  : Unbounded_Wide_String;
      Right : Wide_String) return Boolean
   is
      LR : constant Shared_Wide_String_Access := Left.Reference;
   begin
      return LR.Data (1 .. LR.Last) < Right;
   end "<";

   function "<"
     (Left  : Wide_String;
      Right : Unbounded_Wide_String) return Boolean
   is
      RR : constant Shared_Wide_String_Access := Right.Reference;
   begin
      return Left < RR.Data (1 .. RR.Last);
   end "<";

   ----------
   -- "<=" --
   ----------

   function "<="
     (Left  : Unbounded_Wide_String;
      Right : Unbounded_Wide_String) return Boolean
   is
      LR : constant Shared_Wide_String_Access := Left.Reference;
      RR : constant Shared_Wide_String_Access := Right.Reference;

   begin
      --  LR = RR means two strings shares shared string, thus they are equal

      return LR = RR or else LR.Data (1 .. LR.Last) <= RR.Data (1 .. RR.Last);
   end "<=";

   function "<="
     (Left  : Unbounded_Wide_String;
      Right : Wide_String) return Boolean
   is
      LR : constant Shared_Wide_String_Access := Left.Reference;
   begin
      return LR.Data (1 .. LR.Last) <= Right;
   end "<=";

   function "<="
     (Left  : Wide_String;
      Right : Unbounded_Wide_String) return Boolean
   is
      RR : constant Shared_Wide_String_Access := Right.Reference;
   begin
      return Left <= RR.Data (1 .. RR.Last);
   end "<=";

   ---------
   -- "=" --
   ---------

   function "="
     (Left  : Unbounded_Wide_String;
      Right : Unbounded_Wide_String) return Boolean
   is
      LR : constant Shared_Wide_String_Access := Left.Reference;
      RR : constant Shared_Wide_String_Access := Right.Reference;

   begin
      return LR = RR or else LR.Data (1 .. LR.Last) = RR.Data (1 .. RR.Last);
      --  LR = RR means two strings shares shared string, thus they are equal
   end "=";

   function "="
     (Left  : Unbounded_Wide_String;
      Right : Wide_String) return Boolean
   is
      LR : constant Shared_Wide_String_Access := Left.Reference;
   begin
      return LR.Data (1 .. LR.Last) = Right;
   end "=";

   function "="
     (Left  : Wide_String;
      Right : Unbounded_Wide_String) return Boolean
   is
      RR : constant Shared_Wide_String_Access := Right.Reference;
   begin
      return Left = RR.Data (1 .. RR.Last);
   end "=";

   ---------
   -- ">" --
   ---------

   function ">"
     (Left  : Unbounded_Wide_String;
      Right : Unbounded_Wide_String) return Boolean
   is
      LR : constant Shared_Wide_String_Access := Left.Reference;
      RR : constant Shared_Wide_String_Access := Right.Reference;
   begin
      return LR.Data (1 .. LR.Last) > RR.Data (1 .. RR.Last);
   end ">";

   function ">"
     (Left  : Unbounded_Wide_String;
      Right : Wide_String) return Boolean
   is
      LR : constant Shared_Wide_String_Access := Left.Reference;
   begin
      return LR.Data (1 .. LR.Last) > Right;
   end ">";

   function ">"
     (Left  : Wide_String;
      Right : Unbounded_Wide_String) return Boolean
   is
      RR : constant Shared_Wide_String_Access := Right.Reference;
   begin
      return Left > RR.Data (1 .. RR.Last);
   end ">";

   ----------
   -- ">=" --
   ----------

   function ">="
     (Left  : Unbounded_Wide_String;
      Right : Unbounded_Wide_String) return Boolean
   is
      LR : constant Shared_Wide_String_Access := Left.Reference;
      RR : constant Shared_Wide_String_Access := Right.Reference;

   begin
      --  LR = RR means two strings shares shared string, thus they are equal

      return LR = RR or else LR.Data (1 .. LR.Last) >= RR.Data (1 .. RR.Last);
   end ">=";

   function ">="
     (Left  : Unbounded_Wide_String;
      Right : Wide_String) return Boolean
   is
      LR : constant Shared_Wide_String_Access := Left.Reference;
   begin
      return LR.Data (1 .. LR.Last) >= Right;
   end ">=";

   function ">="
     (Left  : Wide_String;
      Right : Unbounded_Wide_String) return Boolean
   is
      RR : constant Shared_Wide_String_Access := Right.Reference;
   begin
      return Left >= RR.Data (1 .. RR.Last);
   end ">=";

   ------------
   -- Adjust --
   ------------

   procedure Adjust (Object : in out Unbounded_Wide_String) is
   begin
      Reference (Object.Reference);
   end Adjust;

   ------------------------
   -- Aligned_Max_Length --
   ------------------------

   function Aligned_Max_Length (Max_Length : Natural) return Natural is
      Static_Size  : constant Natural :=
        Empty_Shared_Wide_String'Size / Standard'Storage_Unit;
      --  Total size of all static components

      Element_Size : constant Natural :=
        Wide_Character'Size / Standard'Storage_Unit;

   begin
      return
        (((Static_Size + Max_Length * Element_Size - 1) / Min_Mul_Alloc + 2)
          * Min_Mul_Alloc - Static_Size) / Element_Size;
   end Aligned_Max_Length;

   --------------
   -- Allocate --
   --------------

   function Allocate (Max_Length : Natural) return Shared_Wide_String_Access is
   begin
      --  Empty string requested, return shared empty string

      if Max_Length = 0 then
         Reference (Empty_Shared_Wide_String'Access);
         return Empty_Shared_Wide_String'Access;

      --  Otherwise, allocate requested space (and probably some more room)

      else
         return new Shared_Wide_String (Aligned_Max_Length (Max_Length));
      end if;
   end Allocate;

   ------------
   -- Append --
   ------------

   procedure Append
     (Source   : in out Unbounded_Wide_String;
      New_Item : Unbounded_Wide_String)
   is
      SR  : constant Shared_Wide_String_Access := Source.Reference;
      NR  : constant Shared_Wide_String_Access := New_Item.Reference;
      DL  : constant Natural                   := SR.Last + NR.Last;
      DR  : Shared_Wide_String_Access;

   begin
      --  Source is an empty string, reuse New_Item data

      if SR.Last = 0 then
         Reference (NR);
         Source.Reference := NR;
         Unreference (SR);

      --  New_Item is empty string, nothing to do

      elsif NR.Last = 0 then
         null;

      --  Try to reuse existent shared string

      elsif Can_Be_Reused (SR, DL) then
         SR.Data (SR.Last + 1 .. DL) := NR.Data (1 .. NR.Last);
         SR.Last := DL;

      --  Otherwise, allocate new one and fill it

      else
         DR := Allocate (DL + DL / Growth_Factor);
         DR.Data (1 .. SR.Last) := SR.Data (1 .. SR.Last);
         DR.Data (SR.Last + 1 .. DL) := NR.Data (1 .. NR.Last);
         DR.Last := DL;
         Source.Reference := DR;
         Unreference (SR);
      end if;
   end Append;

   procedure Append
     (Source   : in out Unbounded_Wide_String;
      New_Item : Wide_String)
   is
      SR : constant Shared_Wide_String_Access := Source.Reference;
      DL : constant Natural                   := SR.Last + New_Item'Length;
      DR : Shared_Wide_String_Access;

   begin
      --  New_Item is an empty string, nothing to do

      if New_Item'Length = 0 then
         null;

      --  Try to reuse existing shared string

      elsif Can_Be_Reused (SR, DL) then
         SR.Data (SR.Last + 1 .. DL) := New_Item;
         SR.Last := DL;

      --  Otherwise, allocate new one and fill it

      else
         DR := Allocate (DL + DL / Growth_Factor);
         DR.Data (1 .. SR.Last) := SR.Data (1 .. SR.Last);
         DR.Data (SR.Last + 1 .. DL) := New_Item;
         DR.Last := DL;
         Source.Reference := DR;
         Unreference (SR);
      end if;
   end Append;

   procedure Append
     (Source   : in out Unbounded_Wide_String;
      New_Item : Wide_Character)
   is
      SR : constant Shared_Wide_String_Access := Source.Reference;
      DL : constant Natural := SR.Last + 1;
      DR : Shared_Wide_String_Access;

   begin
      --  Try to reuse existing shared string

      if Can_Be_Reused (SR, SR.Last + 1) then
         SR.Data (SR.Last + 1) := New_Item;
         SR.Last := SR.Last + 1;

      --  Otherwise, allocate new one and fill it

      else
         DR := Allocate (DL + DL / Growth_Factor);
         DR.Data (1 .. SR.Last) := SR.Data (1 .. SR.Last);
         DR.Data (DL) := New_Item;
         DR.Last := DL;
         Source.Reference := DR;
         Unreference (SR);
      end if;
   end Append;

   -------------------
   -- Can_Be_Reused --
   -------------------

   function Can_Be_Reused
     (Item   : Shared_Wide_String_Access;
      Length : Natural) return Boolean is
   begin
      return
        System.Atomic_Counters.Is_One (Item.Counter)
          and then Item.Max_Length >= Length
          and then Item.Max_Length <=
                     Aligned_Max_Length (Length + Length / Growth_Factor);
   end Can_Be_Reused;

   -----------
   -- Count --
   -----------

   function Count
     (Source  : Unbounded_Wide_String;
      Pattern : Wide_String;
      Mapping : Wide_Maps.Wide_Character_Mapping := Wide_Maps.Identity)
      return Natural
   is
      SR : constant Shared_Wide_String_Access := Source.Reference;
   begin
      return Wide_Search.Count (SR.Data (1 .. SR.Last), Pattern, Mapping);
   end Count;

   function Count
     (Source  : Unbounded_Wide_String;
      Pattern : Wide_String;
      Mapping : Wide_Maps.Wide_Character_Mapping_Function) return Natural
   is
      SR : constant Shared_Wide_String_Access := Source.Reference;
   begin
      return Wide_Search.Count (SR.Data (1 .. SR.Last), Pattern, Mapping);
   end Count;

   function Count
     (Source : Unbounded_Wide_String;
      Set    : Wide_Maps.Wide_Character_Set) return Natural
   is
      SR : constant Shared_Wide_String_Access := Source.Reference;
   begin
      return Wide_Search.Count (SR.Data (1 .. SR.Last), Set);
   end Count;

   ------------
   -- Delete --
   ------------

   function Delete
     (Source  : Unbounded_Wide_String;
      From    : Positive;
      Through : Natural) return Unbounded_Wide_String
   is
      SR : constant Shared_Wide_String_Access := Source.Reference;
      DL : Natural;
      DR : Shared_Wide_String_Access;

   begin
      --  Empty slice is deleted, use the same shared string

      if From > Through then
         Reference (SR);
         DR := SR;

      --  Index is out of range

      elsif Through > SR.Last then
         raise Index_Error;

      --  Compute size of the result

      else
         DL := SR.Last - (Through - From + 1);

         --  Result is an empty string, reuse shared empty string

         if DL = 0 then
            Reference (Empty_Shared_Wide_String'Access);
            DR := Empty_Shared_Wide_String'Access;

         --  Otherwise, allocate new shared string and fill it

         else
            DR := Allocate (DL);
            DR.Data (1 .. From - 1) := SR.Data (1 .. From - 1);
            DR.Data (From .. DL) := SR.Data (Through + 1 .. SR.Last);
            DR.Last := DL;
         end if;
      end if;

      return (AF.Controlled with Reference => DR);
   end Delete;

   procedure Delete
     (Source  : in out Unbounded_Wide_String;
      From    : Positive;
      Through : Natural)
   is
      SR : constant Shared_Wide_String_Access := Source.Reference;
      DL : Natural;
      DR : Shared_Wide_String_Access;

   begin
      --  Nothing changed, return

      if From > Through then
         null;

      --  Through is outside of the range

      elsif Through > SR.Last then
         raise Index_Error;

      else
         DL := SR.Last - (Through - From + 1);

         --  Result is empty, reuse shared empty string

         if DL = 0 then
            Reference (Empty_Shared_Wide_String'Access);
            Source.Reference := Empty_Shared_Wide_String'Access;
            Unreference (SR);

         --  Try to reuse existent shared string

         elsif Can_Be_Reused (SR, DL) then
            SR.Data (From .. DL) := SR.Data (Through + 1 .. SR.Last);
            SR.Last := DL;

         --  Otherwise, allocate new shared string

         else
            DR := Allocate (DL);
            DR.Data (1 .. From - 1) := SR.Data (1 .. From - 1);
            DR.Data (From .. DL) := SR.Data (Through + 1 .. SR.Last);
            DR.Last := DL;
            Source.Reference := DR;
            Unreference (SR);
         end if;
      end if;
   end Delete;

   -------------
   -- Element --
   -------------

   function Element
     (Source : Unbounded_Wide_String;
      Index  : Positive) return Wide_Character
   is
      SR : constant Shared_Wide_String_Access := Source.Reference;
   begin
      if Index <= SR.Last then
         return SR.Data (Index);
      else
         raise Index_Error;
      end if;
   end Element;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Object : in out Unbounded_Wide_String) is
      SR : constant Shared_Wide_String_Access := Object.Reference;

   begin
      if SR /= null then

         --  The same controlled object can be finalized several times for
         --  some reason. As per 7.6.1(24) this should have no ill effect,
         --  so we need to add a guard for the case of finalizing the same
         --  object twice.

         Object.Reference := null;
         Unreference (SR);
      end if;
   end Finalize;

   ----------------
   -- Find_Token --
   ----------------

   procedure Find_Token
     (Source : Unbounded_Wide_String;
      Set    : Wide_Maps.Wide_Character_Set;
      From   : Positive;
      Test   : Strings.Membership;
      First  : out Positive;
      Last   : out Natural)
   is
      SR : constant Shared_Wide_String_Access := Source.Reference;
   begin
      Wide_Search.Find_Token
        (SR.Data (From .. SR.Last), Set, Test, First, Last);
   end Find_Token;

   procedure Find_Token
     (Source : Unbounded_Wide_String;
      Set    : Wide_Maps.Wide_Character_Set;
      Test   : Strings.Membership;
      First  : out Positive;
      Last   : out Natural)
   is
      SR : constant Shared_Wide_String_Access := Source.Reference;
   begin
      Wide_Search.Find_Token
        (SR.Data (1 .. SR.Last), Set, Test, First, Last);
   end Find_Token;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Wide_String_Access) is
      procedure Deallocate is
         new Ada.Unchecked_Deallocation (Wide_String, Wide_String_Access);
   begin
      Deallocate (X);
   end Free;

   ----------
   -- Head --
   ----------

   function Head
     (Source : Unbounded_Wide_String;
      Count  : Natural;
      Pad    : Wide_Character := Wide_Space) return Unbounded_Wide_String
   is
      SR : constant Shared_Wide_String_Access := Source.Reference;
      DR : Shared_Wide_String_Access;

   begin
      --  Result is empty, reuse shared empty string

      if Count = 0 then
         Reference (Empty_Shared_Wide_String'Access);
         DR := Empty_Shared_Wide_String'Access;

      --  Length of the string is the same as requested, reuse source shared
      --  string.

      elsif Count = SR.Last then
         Reference (SR);
         DR := SR;

      --  Otherwise, allocate new shared string and fill it

      else
         DR := Allocate (Count);

         --  Length of the source string is more than requested, copy
         --  corresponding slice.

         if Count < SR.Last then
            DR.Data (1 .. Count) := SR.Data (1 .. Count);

         --  Length of the source string is less than requested, copy all
         --  contents and fill others by Pad character.

         else
            DR.Data (1 .. SR.Last) := SR.Data (1 .. SR.Last);

            for J in SR.Last + 1 .. Count loop
               DR.Data (J) := Pad;
            end loop;
         end if;

         DR.Last := Count;
      end if;

      return (AF.Controlled with Reference => DR);
   end Head;

   procedure Head
     (Source : in out Unbounded_Wide_String;
      Count  : Natural;
      Pad    : Wide_Character := Wide_Space)
   is
      SR : constant Shared_Wide_String_Access := Source.Reference;
      DR : Shared_Wide_String_Access;

   begin
      --  Result is empty, reuse empty shared string

      if Count = 0 then
         Reference (Empty_Shared_Wide_String'Access);
         Source.Reference := Empty_Shared_Wide_String'Access;
         Unreference (SR);

      --  Result is same with source string, reuse source shared string

      elsif Count = SR.Last then
         null;

      --  Try to reuse existent shared string

      elsif Can_Be_Reused (SR, Count) then
         if Count > SR.Last then
            for J in SR.Last + 1 .. Count loop
               SR.Data (J) := Pad;
            end loop;
         end if;

         SR.Last := Count;

      --  Otherwise, allocate new shared string and fill it

      else
         DR := Allocate (Count);

         --  Length of the source string is greater than requested, copy
         --  corresponding slice.

         if Count < SR.Last then
            DR.Data (1 .. Count) := SR.Data (1 .. Count);

         --  Length of the source string is less than requested, copy all
         --  exists data and fill others by Pad character.

         else
            DR.Data (1 .. SR.Last) := SR.Data (1 .. SR.Last);

            for J in SR.Last + 1 .. Count loop
               DR.Data (J) := Pad;
            end loop;
         end if;

         DR.Last := Count;
         Source.Reference := DR;
         Unreference (SR);
      end if;
   end Head;

   -----------
   -- Index --
   -----------

   function Index
     (Source  : Unbounded_Wide_String;
      Pattern : Wide_String;
      Going   : Strings.Direction := Strings.Forward;
      Mapping : Wide_Maps.Wide_Character_Mapping := Wide_Maps.Identity)
      return Natural
   is
      SR : constant Shared_Wide_String_Access := Source.Reference;
   begin
      return Wide_Search.Index
        (SR.Data (1 .. SR.Last), Pattern, Going, Mapping);
   end Index;

   function Index
     (Source  : Unbounded_Wide_String;
      Pattern : Wide_String;
      Going   : Direction := Forward;
      Mapping : Wide_Maps.Wide_Character_Mapping_Function) return Natural
   is
      SR : constant Shared_Wide_String_Access := Source.Reference;
   begin
      return Wide_Search.Index
        (SR.Data (1 .. SR.Last), Pattern, Going, Mapping);
   end Index;

   function Index
     (Source : Unbounded_Wide_String;
      Set    : Wide_Maps.Wide_Character_Set;
      Test   : Strings.Membership := Strings.Inside;
      Going  : Strings.Direction  := Strings.Forward) return Natural
   is
      SR : constant Shared_Wide_String_Access := Source.Reference;
   begin
      return Wide_Search.Index (SR.Data (1 .. SR.Last), Set, Test, Going);
   end Index;

   function Index
     (Source  : Unbounded_Wide_String;
      Pattern : Wide_String;
      From    : Positive;
      Going   : Direction := Forward;
      Mapping : Wide_Maps.Wide_Character_Mapping := Wide_Maps.Identity)
      return Natural
   is
      SR : constant Shared_Wide_String_Access := Source.Reference;
   begin
      return Wide_Search.Index
        (SR.Data (1 .. SR.Last), Pattern, From, Going, Mapping);
   end Index;

   function Index
     (Source  : Unbounded_Wide_String;
      Pattern : Wide_String;
      From    : Positive;
      Going   : Direction := Forward;
      Mapping : Wide_Maps.Wide_Character_Mapping_Function) return Natural
   is
      SR : constant Shared_Wide_String_Access := Source.Reference;
   begin
      return Wide_Search.Index
        (SR.Data (1 .. SR.Last), Pattern, From, Going, Mapping);
   end Index;

   function Index
     (Source  : Unbounded_Wide_String;
      Set     : Wide_Maps.Wide_Character_Set;
      From    : Positive;
      Test    : Membership := Inside;
      Going   : Direction := Forward) return Natural
   is
      SR : constant Shared_Wide_String_Access := Source.Reference;
   begin
      return Wide_Search.Index
        (SR.Data (1 .. SR.Last), Set, From, Test, Going);
   end Index;

   ---------------------
   -- Index_Non_Blank --
   ---------------------

   function Index_Non_Blank
     (Source : Unbounded_Wide_String;
      Going  : Strings.Direction := Strings.Forward) return Natural
   is
      SR : constant Shared_Wide_String_Access := Source.Reference;
   begin
      return Wide_Search.Index_Non_Blank (SR.Data (1 .. SR.Last), Going);
   end Index_Non_Blank;

   function Index_Non_Blank
     (Source : Unbounded_Wide_String;
      From   : Positive;
      Going  : Direction := Forward) return Natural
   is
      SR : constant Shared_Wide_String_Access := Source.Reference;
   begin
      return Wide_Search.Index_Non_Blank
        (SR.Data (1 .. SR.Last), From, Going);
   end Index_Non_Blank;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Object : in out Unbounded_Wide_String) is
   begin
      Reference (Object.Reference);
   end Initialize;

   ------------
   -- Insert --
   ------------

   function Insert
     (Source   : Unbounded_Wide_String;
      Before   : Positive;
      New_Item : Wide_String) return Unbounded_Wide_String
   is
      SR : constant Shared_Wide_String_Access := Source.Reference;
      DL : constant Natural := SR.Last + New_Item'Length;
      DR : Shared_Wide_String_Access;

   begin
      --  Check index first

      if Before > SR.Last + 1 then
         raise Index_Error;
      end if;

      --  Result is empty, reuse empty shared string

      if DL = 0 then
         Reference (Empty_Shared_Wide_String'Access);
         DR := Empty_Shared_Wide_String'Access;

      --  Inserted string is empty, reuse source shared string

      elsif New_Item'Length = 0 then
         Reference (SR);
         DR := SR;

      --  Otherwise, allocate new shared string and fill it

      else
         DR := Allocate (DL + DL / Growth_Factor);
         DR.Data (1 .. Before - 1) := SR.Data (1 .. Before - 1);
         DR.Data (Before .. Before + New_Item'Length - 1) := New_Item;
         DR.Data (Before + New_Item'Length .. DL) :=
           SR.Data (Before .. SR.Last);
         DR.Last := DL;
      end if;

      return (AF.Controlled with Reference => DR);
   end Insert;

   procedure Insert
     (Source   : in out Unbounded_Wide_String;
      Before   : Positive;
      New_Item : Wide_String)
   is
      SR : constant Shared_Wide_String_Access := Source.Reference;
      DL : constant Natural                   := SR.Last + New_Item'Length;
      DR : Shared_Wide_String_Access;

   begin
      --  Check bounds

      if Before > SR.Last + 1 then
         raise Index_Error;
      end if;

      --  Result is empty string, reuse empty shared string

      if DL = 0 then
         Reference (Empty_Shared_Wide_String'Access);
         Source.Reference := Empty_Shared_Wide_String'Access;
         Unreference (SR);

      --  Inserted string is empty, nothing to do

      elsif New_Item'Length = 0 then
         null;

      --  Try to reuse existent shared string first

      elsif Can_Be_Reused (SR, DL) then
         SR.Data (Before + New_Item'Length .. DL) :=
           SR.Data (Before .. SR.Last);
         SR.Data (Before .. Before + New_Item'Length - 1) := New_Item;
         SR.Last := DL;

      --  Otherwise, allocate new shared string and fill it

      else
         DR := Allocate (DL + DL / Growth_Factor);
         DR.Data (1 .. Before - 1) := SR.Data (1 .. Before - 1);
         DR.Data (Before .. Before + New_Item'Length - 1) := New_Item;
         DR.Data (Before + New_Item'Length .. DL) :=
           SR.Data (Before .. SR.Last);
         DR.Last := DL;
         Source.Reference := DR;
         Unreference (SR);
      end if;
   end Insert;

   ------------
   -- Length --
   ------------

   function Length (Source : Unbounded_Wide_String) return Natural is
   begin
      return Source.Reference.Last;
   end Length;

   ---------------
   -- Overwrite --
   ---------------

   function Overwrite
     (Source   : Unbounded_Wide_String;
      Position : Positive;
      New_Item : Wide_String) return Unbounded_Wide_String
   is
      SR : constant Shared_Wide_String_Access := Source.Reference;
      DL : Natural;
      DR : Shared_Wide_String_Access;

   begin
      --  Check bounds

      if Position > SR.Last + 1 then
         raise Index_Error;
      end if;

      DL := Integer'Max (SR.Last, Position + New_Item'Length - 1);

      --  Result is empty string, reuse empty shared string

      if DL = 0 then
         Reference (Empty_Shared_Wide_String'Access);
         DR := Empty_Shared_Wide_String'Access;

      --  Result is same with source string, reuse source shared string

      elsif New_Item'Length = 0 then
         Reference (SR);
         DR := SR;

      --  Otherwise, allocate new shared string and fill it

      else
         DR := Allocate (DL);
         DR.Data (1 .. Position - 1) := SR.Data (1 .. Position - 1);
         DR.Data (Position .. Position + New_Item'Length - 1) := New_Item;
         DR.Data (Position + New_Item'Length .. DL) :=
           SR.Data (Position + New_Item'Length .. SR.Last);
         DR.Last := DL;
      end if;

      return (AF.Controlled with Reference => DR);
   end Overwrite;

   procedure Overwrite
     (Source    : in out Unbounded_Wide_String;
      Position  : Positive;
      New_Item  : Wide_String)
   is
      SR : constant Shared_Wide_String_Access := Source.Reference;
      DL : Natural;
      DR : Shared_Wide_String_Access;

   begin
      --  Bounds check

      if Position > SR.Last + 1 then
         raise Index_Error;
      end if;

      DL := Integer'Max (SR.Last, Position + New_Item'Length - 1);

      --  Result is empty string, reuse empty shared string

      if DL = 0 then
         Reference (Empty_Shared_Wide_String'Access);
         Source.Reference := Empty_Shared_Wide_String'Access;
         Unreference (SR);

      --  String unchanged, nothing to do

      elsif New_Item'Length = 0 then
         null;

      --  Try to reuse existent shared string

      elsif Can_Be_Reused (SR, DL) then
         SR.Data (Position .. Position + New_Item'Length - 1) := New_Item;
         SR.Last := DL;

      --  Otherwise allocate new shared string and fill it

      else
         DR := Allocate (DL);
         DR.Data (1 .. Position - 1) := SR.Data (1 .. Position - 1);
         DR.Data (Position .. Position + New_Item'Length - 1) := New_Item;
         DR.Data (Position + New_Item'Length .. DL) :=
           SR.Data (Position + New_Item'Length .. SR.Last);
         DR.Last := DL;
         Source.Reference := DR;
         Unreference (SR);
      end if;
   end Overwrite;

   ---------------
   -- Reference --
   ---------------

   procedure Reference (Item : not null Shared_Wide_String_Access) is
   begin
      System.Atomic_Counters.Increment (Item.Counter);
   end Reference;

   ---------------------
   -- Replace_Element --
   ---------------------

   procedure Replace_Element
     (Source : in out Unbounded_Wide_String;
      Index  : Positive;
      By     : Wide_Character)
   is
      SR : constant Shared_Wide_String_Access := Source.Reference;
      DR : Shared_Wide_String_Access;

   begin
      --  Bounds check

      if Index <= SR.Last then

         --  Try to reuse existent shared string

         if Can_Be_Reused (SR, SR.Last) then
            SR.Data (Index) := By;

         --  Otherwise allocate new shared string and fill it

         else
            DR := Allocate (SR.Last);
            DR.Data (1 .. SR.Last) := SR.Data (1 .. SR.Last);
            DR.Data (Index) := By;
            DR.Last := SR.Last;
            Source.Reference := DR;
            Unreference (SR);
         end if;

      else
         raise Index_Error;
      end if;
   end Replace_Element;

   -------------------
   -- Replace_Slice --
   -------------------

   function Replace_Slice
     (Source : Unbounded_Wide_String;
      Low    : Positive;
      High   : Natural;
      By     : Wide_String) return Unbounded_Wide_String
   is
      SR : constant Shared_Wide_String_Access := Source.Reference;
      DL : Natural;
      DR : Shared_Wide_String_Access;

   begin
      --  Check bounds

      if Low > SR.Last + 1 then
         raise Index_Error;
      end if;

      --  Do replace operation when removed slice is not empty

      if High >= Low then
         DL := By'Length + SR.Last + Low - Integer'Min (High, SR.Last) - 1;
         --  This is the number of characters remaining in the string after
         --  replacing the slice.

         --  Result is empty string, reuse empty shared string

         if DL = 0 then
            Reference (Empty_Shared_Wide_String'Access);
            DR := Empty_Shared_Wide_String'Access;

         --  Otherwise allocate new shared string and fill it

         else
            DR := Allocate (DL);
            DR.Data (1 .. Low - 1) := SR.Data (1 .. Low - 1);
            DR.Data (Low .. Low + By'Length - 1) := By;
            DR.Data (Low + By'Length .. DL) := SR.Data (High + 1 .. SR.Last);
            DR.Last := DL;
         end if;

         return (AF.Controlled with Reference => DR);

      --  Otherwise just insert string

      else
         return Insert (Source, Low, By);
      end if;
   end Replace_Slice;

   procedure Replace_Slice
     (Source : in out Unbounded_Wide_String;
      Low    : Positive;
      High   : Natural;
      By     : Wide_String)
   is
      SR : constant Shared_Wide_String_Access := Source.Reference;
      DL : Natural;
      DR : Shared_Wide_String_Access;

   begin
      --  Bounds check

      if Low > SR.Last + 1 then
         raise Index_Error;
      end if;

      --  Do replace operation only when replaced slice is not empty

      if High >= Low then
         DL := By'Length + SR.Last + Low - Integer'Min (High, SR.Last) - 1;
         --  This is the number of characters remaining in the string after
         --  replacing the slice.

         --  Result is empty string, reuse empty shared string

         if DL = 0 then
            Reference (Empty_Shared_Wide_String'Access);
            Source.Reference := Empty_Shared_Wide_String'Access;
            Unreference (SR);

         --  Try to reuse existent shared string

         elsif Can_Be_Reused (SR, DL) then
            SR.Data (Low + By'Length .. DL) := SR.Data (High + 1 .. SR.Last);
            SR.Data (Low .. Low + By'Length - 1) := By;
            SR.Last := DL;

         --  Otherwise allocate new shared string and fill it

         else
            DR := Allocate (DL);
            DR.Data (1 .. Low - 1) := SR.Data (1 .. Low - 1);
            DR.Data (Low .. Low + By'Length - 1) := By;
            DR.Data (Low + By'Length .. DL) := SR.Data (High + 1 .. SR.Last);
            DR.Last := DL;
            Source.Reference := DR;
            Unreference (SR);
         end if;

      --  Otherwise just insert item

      else
         Insert (Source, Low, By);
      end if;
   end Replace_Slice;

   -------------------------------
   -- Set_Unbounded_Wide_String --
   -------------------------------

   procedure Set_Unbounded_Wide_String
     (Target : out Unbounded_Wide_String;
      Source : Wide_String)
   is
      TR : constant Shared_Wide_String_Access := Target.Reference;
      DR : Shared_Wide_String_Access;

   begin
      --  In case of empty string, reuse empty shared string

      if Source'Length = 0 then
         Reference (Empty_Shared_Wide_String'Access);
         Target.Reference := Empty_Shared_Wide_String'Access;

      else
         --  Try to reuse existent shared string

         if Can_Be_Reused (TR, Source'Length) then
            Reference (TR);
            DR := TR;

         --  Otherwise allocate new shared string

         else
            DR := Allocate (Source'Length);
            Target.Reference := DR;
         end if;

         DR.Data (1 .. Source'Length) := Source;
         DR.Last := Source'Length;
      end if;

      Unreference (TR);
   end Set_Unbounded_Wide_String;

   -----------
   -- Slice --
   -----------

   function Slice
     (Source : Unbounded_Wide_String;
      Low    : Positive;
      High   : Natural) return Wide_String
   is
      SR : constant Shared_Wide_String_Access := Source.Reference;

   begin
      --  Note: test of High > Length is in accordance with AI95-00128

      if Low > SR.Last + 1 or else High > SR.Last then
         raise Index_Error;

      else
         return SR.Data (Low .. High);
      end if;
   end Slice;

   ----------
   -- Tail --
   ----------

   function Tail
     (Source : Unbounded_Wide_String;
      Count  : Natural;
      Pad    : Wide_Character := Wide_Space) return Unbounded_Wide_String
   is
      SR : constant Shared_Wide_String_Access := Source.Reference;
      DR : Shared_Wide_String_Access;

   begin
      --  For empty result reuse empty shared string

      if Count = 0 then
         Reference (Empty_Shared_Wide_String'Access);
         DR := Empty_Shared_Wide_String'Access;

      --  Result is hole source string, reuse source shared string

      elsif Count = SR.Last then
         Reference (SR);
         DR := SR;

      --  Otherwise allocate new shared string and fill it

      else
         DR := Allocate (Count);

         if Count < SR.Last then
            DR.Data (1 .. Count) := SR.Data (SR.Last - Count + 1 .. SR.Last);

         else
            for J in 1 .. Count - SR.Last loop
               DR.Data (J) := Pad;
            end loop;

            DR.Data (Count - SR.Last + 1 .. Count) := SR.Data (1 .. SR.Last);
         end if;

         DR.Last := Count;
      end if;

      return (AF.Controlled with Reference => DR);
   end Tail;

   procedure Tail
     (Source : in out Unbounded_Wide_String;
      Count  : Natural;
      Pad    : Wide_Character := Wide_Space)
   is
      SR : constant Shared_Wide_String_Access := Source.Reference;
      DR : Shared_Wide_String_Access;

      procedure Common
        (SR    : Shared_Wide_String_Access;
         DR    : Shared_Wide_String_Access;
         Count : Natural);
      --  Common code of tail computation. SR/DR can point to the same object

      ------------
      -- Common --
      ------------

      procedure Common
        (SR    : Shared_Wide_String_Access;
         DR    : Shared_Wide_String_Access;
         Count : Natural) is
      begin
         if Count < SR.Last then
            DR.Data (1 .. Count) := SR.Data (SR.Last - Count + 1 .. SR.Last);

         else
            DR.Data (Count - SR.Last + 1 .. Count) := SR.Data (1 .. SR.Last);

            for J in 1 .. Count - SR.Last loop
               DR.Data (J) := Pad;
            end loop;
         end if;

         DR.Last := Count;
      end Common;

   begin
      --  Result is empty string, reuse empty shared string

      if Count = 0 then
         Reference (Empty_Shared_Wide_String'Access);
         Source.Reference := Empty_Shared_Wide_String'Access;
         Unreference (SR);

      --  Length of the result is the same with length of the source string,
      --  reuse source shared string.

      elsif Count = SR.Last then
         null;

      --  Try to reuse existent shared string

      elsif Can_Be_Reused (SR, Count) then
         Common (SR, SR, Count);

      --  Otherwise allocate new shared string and fill it

      else
         DR := Allocate (Count);
         Common (SR, DR, Count);
         Source.Reference := DR;
         Unreference (SR);
      end if;
   end Tail;

   --------------------
   -- To_Wide_String --
   --------------------

   function To_Wide_String
     (Source : Unbounded_Wide_String) return Wide_String is
   begin
      return Source.Reference.Data (1 .. Source.Reference.Last);
   end To_Wide_String;

   ------------------------------
   -- To_Unbounded_Wide_String --
   ------------------------------

   function To_Unbounded_Wide_String
     (Source : Wide_String) return Unbounded_Wide_String
   is
      DR : constant Shared_Wide_String_Access := Allocate (Source'Length);
   begin
      DR.Data (1 .. Source'Length) := Source;
      DR.Last := Source'Length;
      return (AF.Controlled with Reference => DR);
   end To_Unbounded_Wide_String;

   function To_Unbounded_Wide_String
     (Length : Natural) return Unbounded_Wide_String
   is
      DR : constant Shared_Wide_String_Access := Allocate (Length);
   begin
      DR.Last := Length;
      return (AF.Controlled with Reference => DR);
   end To_Unbounded_Wide_String;

   ---------------
   -- Translate --
   ---------------

   function Translate
     (Source  : Unbounded_Wide_String;
      Mapping : Wide_Maps.Wide_Character_Mapping) return Unbounded_Wide_String
   is
      SR : constant Shared_Wide_String_Access := Source.Reference;
      DR : Shared_Wide_String_Access;

   begin
      --  Nothing to translate, reuse empty shared string

      if SR.Last = 0 then
         Reference (Empty_Shared_Wide_String'Access);
         DR := Empty_Shared_Wide_String'Access;

      --  Otherwise, allocate new shared string and fill it

      else
         DR := Allocate (SR.Last);

         for J in 1 .. SR.Last loop
            DR.Data (J) := Value (Mapping, SR.Data (J));
         end loop;

         DR.Last := SR.Last;
      end if;

      return (AF.Controlled with Reference => DR);
   end Translate;

   procedure Translate
     (Source  : in out Unbounded_Wide_String;
      Mapping : Wide_Maps.Wide_Character_Mapping)
   is
      SR : constant Shared_Wide_String_Access := Source.Reference;
      DR : Shared_Wide_String_Access;

   begin
      --  Nothing to translate

      if SR.Last = 0 then
         null;

      --  Try to reuse shared string

      elsif Can_Be_Reused (SR, SR.Last) then
         for J in 1 .. SR.Last loop
            SR.Data (J) := Value (Mapping, SR.Data (J));
         end loop;

      --  Otherwise, allocate new shared string

      else
         DR := Allocate (SR.Last);

         for J in 1 .. SR.Last loop
            DR.Data (J) := Value (Mapping, SR.Data (J));
         end loop;

         DR.Last := SR.Last;
         Source.Reference := DR;
         Unreference (SR);
      end if;
   end Translate;

   function Translate
     (Source  : Unbounded_Wide_String;
      Mapping : Wide_Maps.Wide_Character_Mapping_Function)
      return Unbounded_Wide_String
   is
      SR : constant Shared_Wide_String_Access := Source.Reference;
      DR : Shared_Wide_String_Access;

   begin
      --  Nothing to translate, reuse empty shared string

      if SR.Last = 0 then
         Reference (Empty_Shared_Wide_String'Access);
         DR := Empty_Shared_Wide_String'Access;

      --  Otherwise, allocate new shared string and fill it

      else
         DR := Allocate (SR.Last);

         for J in 1 .. SR.Last loop
            DR.Data (J) := Mapping.all (SR.Data (J));
         end loop;

         DR.Last := SR.Last;
      end if;

      return (AF.Controlled with Reference => DR);

   exception
      when others =>
         Unreference (DR);

         raise;
   end Translate;

   procedure Translate
     (Source  : in out Unbounded_Wide_String;
      Mapping : Wide_Maps.Wide_Character_Mapping_Function)
   is
      SR : constant Shared_Wide_String_Access := Source.Reference;
      DR : Shared_Wide_String_Access;

   begin
      --  Nothing to translate

      if SR.Last = 0 then
         null;

      --  Try to reuse shared string

      elsif Can_Be_Reused (SR, SR.Last) then
         for J in 1 .. SR.Last loop
            SR.Data (J) := Mapping.all (SR.Data (J));
         end loop;

      --  Otherwise allocate new shared string and fill it

      else
         DR := Allocate (SR.Last);

         for J in 1 .. SR.Last loop
            DR.Data (J) := Mapping.all (SR.Data (J));
         end loop;

         DR.Last := SR.Last;
         Source.Reference := DR;
         Unreference (SR);
      end if;

   exception
      when others =>
         if DR /= null then
            Unreference (DR);
         end if;

         raise;
   end Translate;

   ----------
   -- Trim --
   ----------

   function Trim
     (Source : Unbounded_Wide_String;
      Side   : Trim_End) return Unbounded_Wide_String
   is
      SR   : constant Shared_Wide_String_Access := Source.Reference;
      DL   : Natural;
      DR   : Shared_Wide_String_Access;
      Low  : Natural;
      High : Natural;

   begin
      Low := Index_Non_Blank (Source, Forward);

      --  All blanks, reuse empty shared string

      if Low = 0 then
         Reference (Empty_Shared_Wide_String'Access);
         DR := Empty_Shared_Wide_String'Access;

      else
         case Side is
            when Left =>
               High := SR.Last;
               DL   := SR.Last - Low + 1;

            when Right =>
               Low  := 1;
               High := Index_Non_Blank (Source, Backward);
               DL   := High;

            when Both =>
               High := Index_Non_Blank (Source, Backward);
               DL   := High - Low + 1;
         end case;

         --  Length of the result is the same as length of the source string,
         --  reuse source shared string.

         if DL = SR.Last then
            Reference (SR);
            DR := SR;

         --  Otherwise, allocate new shared string

         else
            DR := Allocate (DL);
            DR.Data (1 .. DL) := SR.Data (Low .. High);
            DR.Last := DL;
         end if;
      end if;

      return (AF.Controlled with Reference => DR);
   end Trim;

   procedure Trim
     (Source : in out Unbounded_Wide_String;
      Side   : Trim_End)
   is
      SR   : constant Shared_Wide_String_Access := Source.Reference;
      DL   : Natural;
      DR   : Shared_Wide_String_Access;
      Low  : Natural;
      High : Natural;

   begin
      Low := Index_Non_Blank (Source, Forward);

      --  All blanks, reuse empty shared string

      if Low = 0 then
         Reference (Empty_Shared_Wide_String'Access);
         Source.Reference := Empty_Shared_Wide_String'Access;
         Unreference (SR);

      else
         case Side is
            when Left =>
               High := SR.Last;
               DL   := SR.Last - Low + 1;

            when Right =>
               Low  := 1;
               High := Index_Non_Blank (Source, Backward);
               DL   := High;

            when Both =>
               High := Index_Non_Blank (Source, Backward);
               DL   := High - Low + 1;
         end case;

         --  Length of the result is the same as length of the source string,
         --  nothing to do.

         if DL = SR.Last then
            null;

         --  Try to reuse existent shared string

         elsif Can_Be_Reused (SR, DL) then
            SR.Data (1 .. DL) := SR.Data (Low .. High);
            SR.Last := DL;

         --  Otherwise, allocate new shared string

         else
            DR := Allocate (DL);
            DR.Data (1 .. DL) := SR.Data (Low .. High);
            DR.Last := DL;
            Source.Reference := DR;
            Unreference (SR);
         end if;
      end if;
   end Trim;

   function Trim
     (Source : Unbounded_Wide_String;
      Left   : Wide_Maps.Wide_Character_Set;
      Right  : Wide_Maps.Wide_Character_Set) return Unbounded_Wide_String
   is
      SR   : constant Shared_Wide_String_Access := Source.Reference;
      DL   : Natural;
      DR   : Shared_Wide_String_Access;
      Low  : Natural;
      High : Natural;

   begin
      Low := Index (Source, Left, Outside, Forward);

      --  Source includes only characters from Left set, reuse empty shared
      --  string.

      if Low = 0 then
         Reference (Empty_Shared_Wide_String'Access);
         DR := Empty_Shared_Wide_String'Access;

      else
         High := Index (Source, Right, Outside, Backward);
         DL   := Integer'Max (0, High - Low + 1);

         --  Source includes only characters from Right set or result string
         --  is empty, reuse empty shared string.

         if High = 0 or else DL = 0 then
            Reference (Empty_Shared_Wide_String'Access);
            DR := Empty_Shared_Wide_String'Access;

         --  Otherwise, allocate new shared string and fill it

         else
            DR := Allocate (DL);
            DR.Data (1 .. DL) := SR.Data (Low .. High);
            DR.Last := DL;
         end if;
      end if;

      return (AF.Controlled with Reference => DR);
   end Trim;

   procedure Trim
     (Source : in out Unbounded_Wide_String;
      Left   : Wide_Maps.Wide_Character_Set;
      Right  : Wide_Maps.Wide_Character_Set)
   is
      SR   : constant Shared_Wide_String_Access := Source.Reference;
      DL   : Natural;
      DR   : Shared_Wide_String_Access;
      Low  : Natural;
      High : Natural;

   begin
      Low := Index (Source, Left, Outside, Forward);

      --  Source includes only characters from Left set, reuse empty shared
      --  string.

      if Low = 0 then
         Reference (Empty_Shared_Wide_String'Access);
         Source.Reference := Empty_Shared_Wide_String'Access;
         Unreference (SR);

      else
         High := Index (Source, Right, Outside, Backward);
         DL   := Integer'Max (0, High - Low + 1);

         --  Source includes only characters from Right set or result string
         --  is empty, reuse empty shared string.

         if High = 0 or else DL = 0 then
            Reference (Empty_Shared_Wide_String'Access);
            Source.Reference := Empty_Shared_Wide_String'Access;
            Unreference (SR);

         --  Try to reuse existent shared string

         elsif Can_Be_Reused (SR, DL) then
            SR.Data (1 .. DL) := SR.Data (Low .. High);
            SR.Last := DL;

         --  Otherwise, allocate new shared string and fill it

         else
            DR := Allocate (DL);
            DR.Data (1 .. DL) := SR.Data (Low .. High);
            DR.Last := DL;
            Source.Reference := DR;
            Unreference (SR);
         end if;
      end if;
   end Trim;

   ---------------------
   -- Unbounded_Slice --
   ---------------------

   function Unbounded_Slice
     (Source : Unbounded_Wide_String;
      Low    : Positive;
      High   : Natural) return Unbounded_Wide_String
   is
      SR : constant Shared_Wide_String_Access := Source.Reference;
      DL : Natural;
      DR : Shared_Wide_String_Access;

   begin
      --  Check bounds

      if Low > SR.Last + 1 or else High > SR.Last then
         raise Index_Error;

      --  Result is empty slice, reuse empty shared string

      elsif Low > High then
         Reference (Empty_Shared_Wide_String'Access);
         DR := Empty_Shared_Wide_String'Access;

      --  Otherwise, allocate new shared string and fill it

      else
         DL := High - Low + 1;
         DR := Allocate (DL);
         DR.Data (1 .. DL) := SR.Data (Low .. High);
         DR.Last := DL;
      end if;

      return (AF.Controlled with Reference => DR);
   end Unbounded_Slice;

   procedure Unbounded_Slice
     (Source : Unbounded_Wide_String;
      Target : out Unbounded_Wide_String;
      Low    : Positive;
      High   : Natural)
   is
      SR : constant Shared_Wide_String_Access := Source.Reference;
      TR : constant Shared_Wide_String_Access := Target.Reference;
      DL : Natural;
      DR : Shared_Wide_String_Access;

   begin
      --  Check bounds

      if Low > SR.Last + 1 or else High > SR.Last then
         raise Index_Error;

      --  Result is empty slice, reuse empty shared string

      elsif Low > High then
         Reference (Empty_Shared_Wide_String'Access);
         Target.Reference := Empty_Shared_Wide_String'Access;
         Unreference (TR);

      else
         DL := High - Low + 1;

         --  Try to reuse existent shared string

         if Can_Be_Reused (TR, DL) then
            TR.Data (1 .. DL) := SR.Data (Low .. High);
            TR.Last := DL;

         --  Otherwise, allocate new shared string and fill it

         else
            DR := Allocate (DL);
            DR.Data (1 .. DL) := SR.Data (Low .. High);
            DR.Last := DL;
            Target.Reference := DR;
            Unreference (TR);
         end if;
      end if;
   end Unbounded_Slice;

   -----------------
   -- Unreference --
   -----------------

   procedure Unreference (Item : not null Shared_Wide_String_Access) is

      procedure Free is
        new Ada.Unchecked_Deallocation
              (Shared_Wide_String, Shared_Wide_String_Access);

      Aux : Shared_Wide_String_Access := Item;

   begin
      if System.Atomic_Counters.Decrement (Aux.Counter) then

         --  Reference counter of Empty_Shared_Wide_String must never reach
         --  zero.

         pragma Assert (Aux /= Empty_Shared_Wide_String'Access);

         Free (Aux);
      end if;
   end Unreference;

end Ada.Strings.Wide_Unbounded;

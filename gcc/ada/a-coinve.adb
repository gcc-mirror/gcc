------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--    A D A . C O N T A I N E R S . I N D E F I N I T E _ V E C T O R S     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2004-2005 Free Software Foundation, Inc.          --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
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
-- This unit has originally being developed by Matthew J Heaney.            --
------------------------------------------------------------------------------

with Ada.Containers.Generic_Array_Sort;
with Ada.Unchecked_Deallocation;
with System;  use type System.Address;

package body Ada.Containers.Indefinite_Vectors is

   type Int is range System.Min_Int .. System.Max_Int;

   procedure Free is
     new Ada.Unchecked_Deallocation (Elements_Type, Elements_Access);

   procedure Free is
     new Ada.Unchecked_Deallocation (Element_Type, Element_Access);

   ---------
   -- "&" --
   ---------

   function "&" (Left, Right : Vector) return Vector is
      LN : constant Count_Type := Length (Left);
      RN : constant Count_Type := Length (Right);

   begin
      if LN = 0 then
         if RN = 0 then
            return Empty_Vector;
         end if;

         declare
            RE : Elements_Type renames
                   Right.Elements (Index_Type'First .. Right.Last);

            Elements : Elements_Access :=
                         new Elements_Type (RE'Range);

         begin
            for I in Elements'Range loop
               begin
                  if RE (I) /= null then
                     Elements (I) := new Element_Type'(RE (I).all);
                  end if;
               exception
                  when others =>
                     for J in Index_Type'First .. Index_Type'Pred (I) loop
                        Free (Elements (J));
                     end loop;

                     Free (Elements);
                     raise;
               end;
            end loop;

            return (Controlled with Elements, Right.Last, 0, 0);
         end;

      end if;

      if RN = 0 then
         declare
            LE : Elements_Type renames
                   Left.Elements (Index_Type'First .. Left.Last);

            Elements : Elements_Access :=
                         new Elements_Type (LE'Range);

         begin
            for I in Elements'Range loop
               begin
                  if LE (I) /= null then
                     Elements (I) := new Element_Type'(LE (I).all);
                  end if;
               exception
                  when others =>
                     for J in Index_Type'First .. Index_Type'Pred (I) loop
                        Free (Elements (J));
                     end loop;

                     Free (Elements);
                     raise;
               end;
            end loop;

            return (Controlled with Elements, Left.Last, 0, 0);
         end;
      end if;

      declare
         Last_As_Int : constant Int'Base :=
                         Int (Index_Type'First) + Int (LN) + Int (RN) - 1;

         Last : constant Index_Type := Index_Type (Last_As_Int);

         LE : Elements_Type renames
                Left.Elements (Index_Type'First .. Left.Last);

         RE : Elements_Type renames
                Right.Elements (Index_Type'First .. Right.Last);

         Elements : Elements_Access :=
                      new Elements_Type (Index_Type'First .. Last);

         I : Index_Type'Base := Index_Type'Pred (Index_Type'First);

      begin
         for LI in LE'Range loop
            I := Index_Type'Succ (I);

            begin
               if LE (LI) /= null then
                  Elements (I) := new Element_Type'(LE (LI).all);
               end if;
            exception
               when others =>
                  for J in Index_Type'First .. Index_Type'Pred (I) loop
                     Free (Elements (J));
                  end loop;

                  Free (Elements);
                  raise;
            end;
         end loop;

         for RI in RE'Range loop
            I := Index_Type'Succ (I);

            begin
               if RE (RI) /= null then
                  Elements (I) := new Element_Type'(RE (RI).all);
               end if;
            exception
               when others =>
                  for J in Index_Type'First .. Index_Type'Pred (I) loop
                     Free (Elements (J));
                  end loop;

                  Free (Elements);
                  raise;
            end;
         end loop;

         return (Controlled with Elements, Last, 0, 0);
      end;
   end "&";

   function "&" (Left : Vector; Right : Element_Type) return Vector is
      LN : constant Count_Type := Length (Left);

   begin
      if LN = 0 then
         declare
            subtype Elements_Subtype is
              Elements_Type (Index_Type'First .. Index_Type'First);

            Elements : Elements_Access := new Elements_Subtype;

         begin
            begin
               Elements (Elements'First) := new Element_Type'(Right);
            exception
               when others =>
                  Free (Elements);
                  raise;
            end;

            return (Controlled with Elements, Index_Type'First, 0, 0);
         end;
      end if;

      declare
         Last_As_Int : constant Int'Base :=
                         Int (Index_Type'First) + Int (LN);

         Last : constant Index_Type := Index_Type (Last_As_Int);

         LE : Elements_Type renames
                Left.Elements (Index_Type'First .. Left.Last);

         Elements : Elements_Access :=
                      new Elements_Type (Index_Type'First .. Last);

      begin
         for I in LE'Range loop
            begin
               if LE (I) /= null then
                  Elements (I) := new Element_Type'(LE (I).all);
               end if;
            exception
               when others =>
                  for J in Index_Type'First .. Index_Type'Pred (I) loop
                     Free (Elements (J));
                  end loop;

                  Free (Elements);
                  raise;
            end;
         end loop;

         begin
            Elements (Elements'Last) := new Element_Type'(Right);
         exception
            when others =>
               declare
                  subtype J_Subtype is Index_Type'Base range
                    Index_Type'First .. Index_Type'Pred (Elements'Last);
               begin
                  for J in J_Subtype loop
                     Free (Elements (J));
                  end loop;
               end;

               Free (Elements);
               raise;
         end;

         return (Controlled with Elements, Last, 0, 0);
      end;
   end "&";

   function "&" (Left : Element_Type; Right : Vector) return Vector is
      RN : constant Count_Type := Length (Right);

   begin
      if RN = 0 then
         declare
            subtype Elements_Subtype is
              Elements_Type (Index_Type'First .. Index_Type'First);

            Elements : Elements_Access := new Elements_Subtype;

         begin
            begin
               Elements (Elements'First) := new Element_Type'(Left);
            exception
               when others =>
                  Free (Elements);
                  raise;
            end;

            return (Controlled with Elements, Index_Type'First, 0, 0);
         end;
      end if;

      declare
         Last_As_Int : constant Int'Base :=
                         Int (Index_Type'First) + Int (RN);

         Last : constant Index_Type := Index_Type (Last_As_Int);

         RE : Elements_Type renames
                Right.Elements (Index_Type'First .. Right.Last);

         Elements : Elements_Access :=
                      new Elements_Type (Index_Type'First .. Last);

         I : Index_Type'Base := Index_Type'First;

      begin
         begin
            Elements (I) := new Element_Type'(Left);
         exception
            when others =>
               Free (Elements);
               raise;
         end;

         for RI in RE'Range loop
            I := Index_Type'Succ (I);

            begin
               if RE (RI) /= null then
                  Elements (I) := new Element_Type'(RE (RI).all);
               end if;
            exception
               when others =>
                  for J in Index_Type'First .. Index_Type'Pred (I) loop
                     Free (Elements (J));
                  end loop;

                  Free (Elements);
                  raise;
            end;
         end loop;

         return (Controlled with Elements, Last, 0, 0);
      end;
   end "&";

   function "&" (Left, Right : Element_Type) return Vector is
      subtype IT is Index_Type'Base range
        Index_Type'First .. Index_Type'Succ (Index_Type'First);

      Elements : Elements_Access := new Elements_Type (IT);

   begin
      begin
         Elements (Elements'First) := new Element_Type'(Left);
      exception
         when others =>
            Free (Elements);
            raise;
      end;

      begin
         Elements (Elements'Last) := new Element_Type'(Right);
      exception
         when others =>
            Free (Elements (Elements'First));
            Free (Elements);
            raise;
      end;

      return (Controlled with Elements, Elements'Last, 0, 0);
   end "&";

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Vector) return Boolean is
   begin
      if Left'Address = Right'Address then
         return True;
      end if;

      if Left.Last /= Right.Last then
         return False;
      end if;

      for J in Index_Type'First .. Left.Last loop
         --  NOTE:
         --  I think it's a bounded error to read or otherwise manipulate
         --  an "empty" element, which here means that it has the value
         --  null.  If it's a bounded error then an exception might
         --  propagate, or it might not.  We take advantage of that
         --  permission here to allow empty elements to be compared.
         --
         --  Whether this is the right decision I'm not really sure.  If
         --  you have a contrary argument then let me know.
         --  END NOTE.

         if Left.Elements (J) = null then
            if Right.Elements (J) /= null then
               return False;
            end if;

         elsif Right.Elements (J) = null then
            return False;

         elsif Left.Elements (J).all /= Right.Elements (J).all then
            return False;

         end if;
      end loop;

      return True;
   end "=";

   ------------
   -- Adjust --
   ------------

   procedure Adjust (Container : in out Vector) is
   begin
      if Container.Elements = null then
         return;
      end if;

      if Container.Elements'Length = 0
        or else Container.Last < Index_Type'First
      then
         Container.Elements := null;
         return;
      end if;

      declare
         E : Elements_Type renames Container.Elements.all;
         L : constant Index_Type := Container.Last;
      begin
         Container.Elements := null;
         Container.Last := No_Index;
         Container.Busy := 0;
         Container.Lock := 0;

         Container.Elements := new Elements_Type (Index_Type'First .. L);

         for I in Container.Elements'Range loop
            if E (I) /= null then
               Container.Elements (I) := new Element_Type'(E (I).all);
            end if;

            Container.Last := I;
         end loop;
      end;
   end Adjust;

   ------------
   -- Append --
   ------------

   procedure Append (Container : in out Vector; New_Item : Vector) is
   begin
      if Is_Empty (New_Item) then
         return;
      end if;

      Insert
        (Container,
         Index_Type'Succ (Container.Last),
         New_Item);
   end Append;

   procedure Append
     (Container : in out Vector;
      New_Item  : Element_Type;
      Count     : Count_Type := 1)
   is
   begin
      if Count = 0 then
         return;
      end if;

      Insert
        (Container,
         Index_Type'Succ (Container.Last),
         New_Item,
         Count);
   end Append;

   ------------
   -- Assign --
   ------------

   procedure Assign
     (Target : in out Vector;
      Source : Vector)
   is
      N : constant Count_Type := Length (Source);

   begin
      if Target'Address = Source'Address then
         return;
      end if;

      Clear (Target);

      if N = 0 then
         return;
      end if;

      if N > Capacity (Target) then
         Reserve_Capacity (Target, Capacity => N);
      end if;

      for J in Index_Type'First .. Source.Last loop
         declare
            EA : constant Element_Access := Source.Elements (J);
         begin
            if EA /= null then
               Target.Elements (J) := new Element_Type'(EA.all);
            end if;
         end;

         Target.Last := J;
      end loop;
   end Assign;

   --------------
   -- Capacity --
   --------------

   function Capacity (Container : Vector) return Count_Type is
   begin
      if Container.Elements = null then
         return 0;
      end if;

      return Container.Elements'Length;
   end Capacity;

   -----------
   -- Clear --
   -----------

   procedure Clear (Container : in out Vector) is
   begin
      if Container.Busy > 0 then
         raise Program_Error;
      end if;

      for J in reverse Index_Type'First .. Container.Last loop
         declare
            X : Element_Access := Container.Elements (J);
         begin
            Container.Elements (J) := null;
            Container.Last := Index_Type'Pred (J);
            Free (X);
         end;
      end loop;
   end Clear;

   --------------
   -- Contains --
   --------------

   function Contains
     (Container : Vector;
      Item      : Element_Type) return Boolean is
   begin
      return Find_Index (Container, Item) /= No_Index;
   end Contains;

   ------------
   -- Delete --
   ------------

   procedure Delete
     (Container : in out Vector;
      Index     : Extended_Index;
      Count     : Count_Type := 1)
   is
   begin
      if Index < Index_Type'First then
         raise Constraint_Error;
      end if;

      if Index > Container.Last then
         if Index > Container.Last + 1 then
            raise Constraint_Error;
         end if;

         return;
      end if;

      if Count = 0 then
         return;
      end if;

      if Container.Busy > 0 then
         raise Program_Error;
      end if;

      declare
         I_As_Int : constant Int := Int (Index);

         Old_Last_As_Int : constant Int := Int (Container.Last);

         Count1 : constant Int'Base := Int (Count);
         Count2 : constant Int'Base := Old_Last_As_Int - I_As_Int + 1;

         N : constant Int'Base := Int'Min (Count1, Count2);

         J_As_Int : constant Int'Base := I_As_Int + N;
         J        : constant Index_Type'Base := Index_Type'Base (J_As_Int);

         E : Elements_Type renames Container.Elements.all;

         New_Last_As_Int : constant Int'Base := Old_Last_As_Int - N;

         New_Last : constant Extended_Index :=
                      Extended_Index (New_Last_As_Int);

      begin
         for K in Index .. Index_Type'Pred (J) loop
            declare
               X : Element_Access := E (K);
            begin
               E (K) := null;
               Free (X);
            end;
         end loop;

         E (Index .. New_Last) := E (J .. Container.Last);
         Container.Last := New_Last;
      end;
   end Delete;

   procedure Delete
     (Container : in out Vector;
      Position  : in out Cursor;
      Count     : Count_Type := 1)
   is
   begin
      if Position.Container = null then
         raise Constraint_Error;
      end if;

      if Position.Container /=
           Vector_Access'(Container'Unchecked_Access)
        or else Position.Index > Container.Last
      then
         raise Program_Error;
      end if;

      Delete (Container, Position.Index, Count);

      if Position.Index <= Container.Last then
         Position := (Container'Unchecked_Access, Position.Index);
      else
         Position := No_Element;
      end if;
   end Delete;

   ------------------
   -- Delete_First --
   ------------------

   procedure Delete_First
     (Container : in out Vector;
      Count     : Count_Type := 1)
   is
   begin
      if Count = 0 then
         return;
      end if;

      if Count >= Length (Container) then
         Clear (Container);
         return;
      end if;

      Delete (Container, Index_Type'First, Count);
   end Delete_First;

   -----------------
   -- Delete_Last --
   -----------------

   procedure Delete_Last
     (Container : in out Vector;
      Count     : Count_Type := 1)
   is
      Index : Int'Base;

   begin
      if Count = 0 then
         return;
      end if;

      if Count >= Length (Container) then
         Clear (Container);
         return;
      end if;

      Index := Int'Base (Container.Last) - Int'Base (Count) + 1;

      Delete (Container, Index_Type'Base (Index), Count);
   end Delete_Last;

   -------------
   -- Element --
   -------------

   function Element
     (Container : Vector;
      Index     : Index_Type) return Element_Type
   is
      subtype T is Index_Type'Base range
        Index_Type'First .. Container.Last;
   begin
      return Container.Elements (T'(Index)).all;
   end Element;

   function Element (Position : Cursor) return Element_Type is
   begin
      return Element (Position.Container.all, Position.Index);
   end Element;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Container : in out Vector) is
   begin
      Clear (Container);

      declare
         X : Elements_Access := Container.Elements;
      begin
         Container.Elements := null;
         Free (X);
      end;
   end Finalize;

   ----------
   -- Find --
   ----------

   function Find
     (Container : Vector;
      Item      : Element_Type;
      Position  : Cursor := No_Element) return Cursor is

   begin
      if Position.Container /= null
        and then (Position.Container /=
                    Vector_Access'(Container'Unchecked_Access)
                  or else Position.Index > Container.Last)
      then
         raise Program_Error;
      end if;

      for J in Position.Index .. Container.Last loop
         if Container.Elements (J) /= null
           and then Container.Elements (J).all = Item
         then
            return (Container'Unchecked_Access, J);
         end if;
      end loop;

      return No_Element;
   end Find;

   ----------------
   -- Find_Index --
   ----------------

   function Find_Index
     (Container : Vector;
      Item      : Element_Type;
      Index     : Index_Type := Index_Type'First) return Extended_Index is
   begin
      for Indx in Index .. Container.Last loop
         if Container.Elements (Indx) /= null
           and then Container.Elements (Indx).all = Item
         then
            return Indx;
         end if;
      end loop;

      return No_Index;
   end Find_Index;

   -----------
   -- First --
   -----------

   function First (Container : Vector) return Cursor is
   begin
      if Is_Empty (Container) then
         return No_Element;
      end if;

      return (Container'Unchecked_Access, Index_Type'First);
   end First;

   -------------------
   -- First_Element --
   -------------------

   function First_Element (Container : Vector) return Element_Type is
   begin
      return Element (Container, Index_Type'First);
   end First_Element;

   -----------------
   -- First_Index --
   -----------------

   function First_Index (Container : Vector) return Index_Type is
      pragma Unreferenced (Container);
   begin
      return Index_Type'First;
   end First_Index;

   ---------------------
   -- Generic_Sorting --
   ---------------------

   package body Generic_Sorting is

      -----------------------
      -- Local Subprograms --
      -----------------------

      function Is_Less (L, R : Element_Access) return Boolean;
      pragma Inline (Is_Less);

      -------------
      -- Is_Less --
      -------------

      function Is_Less (L, R : Element_Access) return Boolean is
      begin
         if L = null then
            return R /= null;
         elsif R = null then
            return False;
         else
            return L.all < R.all;
         end if;
      end Is_Less;

      ---------------
      -- Is_Sorted --
      ---------------

      function Is_Sorted (Container : Vector) return Boolean is
      begin
         if Container.Last <= Index_Type'First then
            return True;
         end if;

         declare
            E : Elements_Type renames Container.Elements.all;
         begin
            for I in Index_Type'First .. Container.Last - 1 loop
               if Is_Less (E (I + 1), E (I)) then
                  return False;
               end if;
            end loop;
         end;

         return True;
      end Is_Sorted;

      -----------
      -- Merge --
      -----------

      procedure Merge (Target, Source : in out Vector) is
         I : Index_Type'Base := Target.Last;
         J : Index_Type'Base;

      begin
         if Target.Last < Index_Type'First then
            Move (Target => Target, Source => Source);
            return;
         end if;

         if Target'Address = Source'Address then
            return;
         end if;

         if Source.Last < Index_Type'First then
            return;
         end if;

         if Source.Busy > 0 then
            raise Program_Error;
         end if;

         Target.Set_Length (Length (Target) + Length (Source));

         J := Target.Last;
         while Source.Last >= Index_Type'First loop
            if I < Index_Type'First then
               declare
                  Src : Elements_Type renames
                    Source.Elements (Index_Type'First .. Source.Last);

               begin
                  Target.Elements (Index_Type'First .. J) := Src;
                  Src := (others => null);
               end;

               Source.Last := No_Index;
               return;
            end if;

            declare
               Src : Element_Access renames Source.Elements (Source.Last);
               Tgt : Element_Access renames Target.Elements (I);

            begin
               if Is_Less (Src, Tgt) then
                  Target.Elements (J) := Tgt;
                  Tgt := null;
                  I := I - 1;

               else
                  Target.Elements (J) := Src;
                  Src := null;
                  Source.Last := Source.Last - 1;
               end if;
            end;

            J := J - 1;
         end loop;
      end Merge;

      ----------
      -- Sort --
      ----------

      procedure Sort (Container : in out Vector)
      is
         procedure Sort is
            new Generic_Array_Sort
             (Index_Type   => Index_Type,
              Element_Type => Element_Access,
              Array_Type   => Elements_Type,
              "<"          => Is_Less);

      --  Start of processing for Sort

      begin
         if Container.Last <= Index_Type'First then
            return;
         end if;

         if Container.Lock > 0 then
            raise Program_Error;
         end if;

         Sort (Container.Elements (Index_Type'First .. Container.Last));
      end Sort;

   end Generic_Sorting;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Position : Cursor) return Boolean is
   begin
      if Position.Container = null then
         return False;
      end if;

      return Position.Index <= Position.Container.Last;
   end Has_Element;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Container : in out Vector;
      Before    : Extended_Index;
      New_Item  : Element_Type;
      Count     : Count_Type := 1)
   is
      N : constant Int := Int (Count);

      New_Last_As_Int : Int'Base;
      New_Last        : Index_Type;

      Index : Extended_Index;  -- TODO: see note in a-convec.adb.

      Dst_Last : Index_Type;
      Dst      : Elements_Access;

   begin
      if Before < Index_Type'First then
         raise Constraint_Error;
      end if;

      if Before > Container.Last
        and then Before > Container.Last + 1
      then
         raise Constraint_Error;
      end if;

      if Count = 0 then
         return;
      end if;

      declare
         Old_Last_As_Int : constant Int := Int (Container.Last);

      begin
         New_Last_As_Int := Old_Last_As_Int + N;
         New_Last := Index_Type (New_Last_As_Int);
      end;

      if Container.Busy > 0 then
         raise Program_Error;
      end if;

      declare
         Old_First_As_Int : constant Int := Int (Before);

         New_First_As_Int : constant Int'Base := Old_First_As_Int + N;

      begin
         Index := Extended_Index (New_First_As_Int);  --  TODO
      end;

      if Container.Elements = null then
         declare
            subtype Elements_Subtype is
              Elements_Type (Index_Type'First .. New_Last);
         begin
            Container.Elements := new Elements_Subtype;
            Container.Last := Index_Type'Pred (Index_Type'First);

            for J in Container.Elements'Range loop
               Container.Elements (J) := new Element_Type'(New_Item);
               Container.Last := J;
            end loop;
         end;

         return;
      end if;

      if New_Last <= Container.Elements'Last then
         declare
            E : Elements_Type renames Container.Elements.all;
         begin
            E (Index .. New_Last) := E (Before .. Container.Last);
            Container.Last := New_Last;

            --  NOTE:
            --  Now we do the allocation.  If it fails, we can propagate the
            --  exception and invariants are more or less satisfied.  The
            --  issue is that we have some slots still null, and the client
            --  has no way of detecting whether the slot is null (unless we
            --  give him a way).
            --
            --  Another way is to allocate a subarray on the stack, do the
            --  allocation into that array, and if that success then do
            --  the insertion proper.  The issue there is that you have to
            --  allocate the subarray on the stack, and that may fail if the
            --  subarray is long.
            --
            --  Or we could try to roll-back the changes: deallocate the
            --  elements we have successfully deallocated, and then copy
            --  the elements ptrs back to their original posns.
            --  END NOTE.

            --  NOTE: I have written the loop manually here.  I could
            --  have done it this way too:
            --    E (Before .. Index_Type'Pred (Index)) :=
            --      (others => new Element_Type'New_Item);
            --  END NOTE.

            for J in Before .. Index_Type'Pred (Index) loop
               begin
                  E (J) := new Element_Type'(New_Item);
               exception
                  when others =>
                     E (J .. Index_Type'Pred (Index)) := (others => null);
                     raise;
               end;
            end loop;
         end;

         return;
      end if;

      declare
         First : constant Int := Int (Index_Type'First);

         New_Size : constant Int'Base := New_Last_As_Int - First + 1;
         Max_Size : constant Int'Base := Int (Index_Type'Last) - First + 1;

         Size, Dst_Last_As_Int : Int'Base;

      begin
         if New_Size >= Max_Size / 2 then
            Dst_Last := Index_Type'Last;

         else
            Size := Container.Elements'Length;

            if Size = 0 then
               Size := 1;
            end if;

            while Size < New_Size loop
               Size := 2 * Size;
            end loop;

            Dst_Last_As_Int := First + Size - 1;
            Dst_Last := Index_Type (Dst_Last_As_Int);
         end if;
      end;

      Dst := new Elements_Type (Index_Type'First .. Dst_Last);

      declare
         Src : Elements_Type renames Container.Elements.all;

      begin
         Dst (Index_Type'First .. Index_Type'Pred (Before)) :=
           Src (Index_Type'First .. Index_Type'Pred (Before));

         Dst (Index .. New_Last) := Src (Before .. Container.Last);
      end;

      declare
         X : Elements_Access := Container.Elements;
      begin
         Container.Elements := Dst;
         Container.Last := New_Last;

         Free (X);
      end;

      --  NOTE:
      --  Now do the allocation.  If the allocation fails,
      --  then the worst thing is that we have a few null slots.
      --  Our invariants are otherwise satisfied.
      --  END NOTE.

      for J in Before .. Index_Type'Pred (Index) loop
         Dst (J) := new Element_Type'(New_Item);
      end loop;
   end Insert;

   procedure Insert
     (Container : in out Vector;
      Before    : Extended_Index;
      New_Item  : Vector)
   is
      N : constant Count_Type := Length (New_Item);

   begin
      if Before < Index_Type'First then
         raise Constraint_Error;
      end if;

      if Before > Container.Last
        and then Before > Container.Last + 1
      then
         raise Constraint_Error;
      end if;

      if N = 0 then
         return;
      end if;

      Insert_Space (Container, Before, Count => N);

      if Container'Address = New_Item'Address then
         declare
            Dst_Last_As_Int : constant Int'Base :=
                                Int'Base (Before) + Int'Base (N) - 1;

            Dst_Last : constant Index_Type := Index_Type (Dst_Last_As_Int);

            Dst_Index : Index_Type'Base := Index_Type'Pred (Before);

            Dst : Elements_Type renames
                    Container.Elements (Before .. Dst_Last);

         begin
            declare
               subtype Src_Index_Subtype is Index_Type'Base range
                 Index_Type'First .. Index_Type'Pred (Before);

               Src : Elements_Type renames
                       Container.Elements (Src_Index_Subtype);

            begin
               for Src_Index in Src'Range loop
                  Dst_Index := Index_Type'Succ (Dst_Index);

                  if Src (Src_Index) /= null then
                     Dst (Dst_Index) := new Element_Type'(Src (Src_Index).all);
                  end if;
               end loop;
            end;

            declare
               subtype Src_Index_Subtype is Index_Type'Base range
                 Index_Type'Succ (Dst_Last) .. Container.Last;

               Src : Elements_Type renames
                       Container.Elements (Src_Index_Subtype);

            begin
               for Src_Index in Src'Range loop
                  Dst_Index := Index_Type'Succ (Dst_Index);

                  if Src (Src_Index) /= null then
                     Dst (Dst_Index) := new Element_Type'(Src (Src_Index).all);
                  end if;
               end loop;
            end;
         end;

      else
         declare
            Dst_Last_As_Int : constant Int'Base :=
                                Int'Base (Before) + Int'Base (N) - 1;

            Dst_Last : constant Index_Type := Index_Type (Dst_Last_As_Int);

            Dst_Index : Index_Type'Base := Index_Type'Pred (Before);

            Src : Elements_Type renames
                    New_Item.Elements (Index_Type'First .. New_Item.Last);

            Dst : Elements_Type renames
                    Container.Elements (Before .. Dst_Last);
         begin
            for Src_Index in Src'Range loop
               Dst_Index := Index_Type'Succ (Dst_Index);

               if Src (Src_Index) /= null then
                  Dst (Dst_Index) := new Element_Type'(Src (Src_Index).all);
               end if;
            end loop;
         end;

      end if;
   end Insert;

   procedure Insert
     (Container : in out Vector;
      Before    : Cursor;
      New_Item  : Vector)
   is
      Index : Index_Type'Base;

   begin
      if Before.Container /= null
        and then Before.Container /= Vector_Access'(Container'Unchecked_Access)
      then
         raise Program_Error;
      end if;

      if Is_Empty (New_Item) then
         return;
      end if;

      if Before.Container = null
        or else Before.Index > Container.Last
      then
         Index := Index_Type'Succ (Container.Last);
      else
         Index := Before.Index;
      end if;

      Insert (Container, Index, New_Item);
   end Insert;

   procedure Insert
     (Container : in out Vector;
      Before    : Cursor;
      New_Item  : Vector;
      Position  : out Cursor)
   is
      Index : Index_Type'Base;

   begin
      if Before.Container /= null
        and then Before.Container /= Vector_Access'(Container'Unchecked_Access)
      then
         raise Program_Error;
      end if;

      if Is_Empty (New_Item) then
         if Before.Container = null
           or else Before.Index > Container.Last
         then
            Position := No_Element;
         else
            Position := (Container'Unchecked_Access, Before.Index);
         end if;

         return;
      end if;

      if Before.Container = null
        or else Before.Index > Container.Last
      then
         Index := Index_Type'Succ (Container.Last);
      else
         Index := Before.Index;
      end if;

      Insert (Container, Index, New_Item);

      Position := Cursor'(Container'Unchecked_Access, Index);
   end Insert;

   procedure Insert
     (Container : in out Vector;
      Before    : Cursor;
      New_Item  : Element_Type;
      Count     : Count_Type := 1)
   is
      Index : Index_Type'Base;

   begin
      if Before.Container /= null
        and then Before.Container /= Vector_Access'(Container'Unchecked_Access)
      then
         raise Program_Error;
      end if;

      if Count = 0 then
         return;
      end if;

      if Before.Container = null
        or else Before.Index > Container.Last
      then
         Index := Index_Type'Succ (Container.Last);
      else
         Index := Before.Index;
      end if;

      Insert (Container, Index, New_Item, Count);
   end Insert;

   procedure Insert
     (Container : in out Vector;
      Before    : Cursor;
      New_Item  : Element_Type;
      Position  : out Cursor;
      Count     : Count_Type := 1)
   is
      Index : Index_Type'Base;

   begin
      if Before.Container /= null
        and then Before.Container /= Vector_Access'(Container'Unchecked_Access)
      then
         raise Program_Error;
      end if;

      if Count = 0 then
         if Before.Container = null
           or else Before.Index > Container.Last
         then
            Position := No_Element;
         else
            Position := (Container'Unchecked_Access, Before.Index);
         end if;

         return;
      end if;

      if Before.Container = null
        or else Before.Index > Container.Last
      then
         Index := Index_Type'Succ (Container.Last);
      else
         Index := Before.Index;
      end if;

      Insert (Container, Index, New_Item, Count);

      Position := (Container'Unchecked_Access, Index);
   end Insert;

   ------------------
   -- Insert_Space --
   ------------------

   procedure Insert_Space
     (Container : in out Vector;
      Before    : Extended_Index;
      Count     : Count_Type := 1)
   is
      N : constant Int := Int (Count);

      New_Last_As_Int : Int'Base;
      New_Last        : Index_Type;

      Index : Extended_Index;  --  TODO: see a-convec.adb.

      Dst_Last : Index_Type;
      Dst      : Elements_Access;

   begin
      if Before < Index_Type'First then
         raise Constraint_Error;
      end if;

      if Before > Container.Last
        and then Before > Container.Last + 1
      then
         raise Constraint_Error;
      end if;

      if Count = 0 then
         return;
      end if;

      declare
         Old_Last_As_Int : constant Int := Int (Container.Last);

      begin
         New_Last_As_Int := Old_Last_As_Int + N;
         New_Last := Index_Type (New_Last_As_Int);
      end;

      if Container.Busy > 0 then
         raise Program_Error;
      end if;

      declare
         Old_First_As_Int : constant Int := Int (Before);

         New_First_As_Int : constant Int'Base := Old_First_As_Int + N;

      begin
         Index := Extended_Index (New_First_As_Int);  --  TODO
      end;

      if Container.Elements = null then
         declare
            subtype Elements_Subtype is
              Elements_Type (Index_Type'First .. New_Last);
         begin
            Container.Elements := new Elements_Subtype;
            Container.Last := New_Last;
         end;

         return;
      end if;

      if New_Last <= Container.Elements'Last then
         declare
            E : Elements_Type renames Container.Elements.all;
         begin
            E (Index .. New_Last) := E (Before .. Container.Last);
            E (Before .. Index_Type'Pred (Index)) := (others => null);

            Container.Last := New_Last;
         end;

         return;
      end if;

      declare
         First : constant Int := Int (Index_Type'First);

         New_Size : constant Int'Base :=
                      Int (New_Last_As_Int) - First + 1;

         Max_Size : constant Int'Base :=
                      Int (Index_Type'Last) - First + 1;

         Size, Dst_Last_As_Int : Int'Base;

      begin
         if New_Size >= Max_Size / 2 then
            Dst_Last := Index_Type'Last;

         else
            Size := Container.Elements'Length;

            if Size = 0 then
               Size := 1;
            end if;

            while Size < New_Size loop
               Size := 2 * Size;
            end loop;

            Dst_Last_As_Int := First + Size - 1;
            Dst_Last := Index_Type (Dst_Last_As_Int);
         end if;
      end;

      Dst := new Elements_Type (Index_Type'First .. Dst_Last);

      declare
         Src : Elements_Type renames Container.Elements.all;

      begin
         Dst (Index_Type'First .. Index_Type'Pred (Before)) :=
           Src (Index_Type'First .. Index_Type'Pred (Before));

         Dst (Index .. New_Last) := Src (Before .. Container.Last);
      end;

      declare
         X : Elements_Access := Container.Elements;
      begin
         Container.Elements := Dst;
         Container.Last := New_Last;

         Free (X);
      end;
   end Insert_Space;

   procedure Insert_Space
     (Container : in out Vector;
      Before    : Cursor;
      Position  : out Cursor;
      Count     : Count_Type := 1)
   is
      Index : Index_Type'Base;

   begin
      if Before.Container /= null
        and then Before.Container /= Vector_Access'(Container'Unchecked_Access)
      then
         raise Program_Error;
      end if;

      if Count = 0 then
         if Before.Container = null
           or else Before.Index > Container.Last
         then
            Position := No_Element;
         else
            Position := (Container'Unchecked_Access, Before.Index);
         end if;

         return;
      end if;

      if Before.Container = null
        or else Before.Index > Container.Last
      then
         Index := Index_Type'Succ (Container.Last);
      else
         Index := Before.Index;
      end if;

      Insert_Space (Container, Index, Count);

      Position := Cursor'(Container'Unchecked_Access, Index);
   end Insert_Space;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Container : Vector) return Boolean is
   begin
      return Container.Last < Index_Type'First;
   end Is_Empty;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Container : Vector;
      Process   : not null access procedure (Position : in Cursor))
   is
      V : Vector renames Container'Unrestricted_Access.all;
      B : Natural renames V.Busy;

   begin
      B := B + 1;

      begin
         for Indx in Index_Type'First .. Container.Last loop
            Process (Cursor'(Container'Unchecked_Access, Indx));
         end loop;
      exception
         when others =>
            B := B - 1;
            raise;
      end;

      B := B - 1;
   end Iterate;

   ----------
   -- Last --
   ----------

   function Last (Container : Vector) return Cursor is
   begin
      if Is_Empty (Container) then
         return No_Element;
      end if;

      return (Container'Unchecked_Access, Container.Last);
   end Last;

   ------------------
   -- Last_Element --
   ------------------

   function Last_Element (Container : Vector) return Element_Type is
   begin
      return Element (Container, Container.Last);
   end Last_Element;

   ----------------
   -- Last_Index --
   ----------------

   function Last_Index (Container : Vector) return Extended_Index is
   begin
      return Container.Last;
   end Last_Index;

   ------------
   -- Length --
   ------------

   function Length (Container : Vector) return Count_Type is
      L : constant Int := Int (Container.Last);
      F : constant Int := Int (Index_Type'First);
      N : constant Int'Base := L - F + 1;
   begin
      return Count_Type (N);
   end Length;

   ----------
   -- Move --
   ----------

   procedure Move
     (Target : in out Vector;
      Source : in out Vector)
   is
   begin
      if Target'Address = Source'Address then
         return;
      end if;

      if Source.Busy > 0 then
         raise Program_Error;
      end if;

      Clear (Target);

      declare
         X : Elements_Access := Target.Elements;
      begin
         Target.Elements := null;
         Free (X);
      end;

      Target.Elements := Source.Elements;
      Target.Last := Source.Last;

      Source.Elements := null;
      Source.Last := No_Index;
   end Move;

   ----------
   -- Next --
   ----------

   function Next (Position : Cursor) return Cursor is
   begin
      if Position.Container = null then
         return No_Element;
      end if;

      if Position.Index < Position.Container.Last then
         return (Position.Container, Index_Type'Succ (Position.Index));
      end if;

      return No_Element;
   end Next;

   ----------
   -- Next --
   ----------

   procedure Next (Position : in out Cursor) is
   begin
      if Position.Container = null then
         return;
      end if;

      if Position.Index < Position.Container.Last then
         Position.Index := Index_Type'Succ (Position.Index);
      else
         Position := No_Element;
      end if;
   end Next;

   -------------
   -- Prepend --
   -------------

   procedure Prepend (Container : in out Vector; New_Item : Vector) is
   begin
      Insert (Container, Index_Type'First, New_Item);
   end Prepend;

   procedure Prepend
     (Container : in out Vector;
      New_Item  : Element_Type;
      Count     : Count_Type := 1)
   is
   begin
      Insert (Container,
              Index_Type'First,
              New_Item,
              Count);
   end Prepend;

   --------------
   -- Previous --
   --------------

   procedure Previous (Position : in out Cursor) is
   begin
      if Position.Container = null then
         return;
      end if;

      if Position.Index > Index_Type'First then
         Position.Index := Index_Type'Pred (Position.Index);
      else
         Position := No_Element;
      end if;
   end Previous;

   function Previous (Position : Cursor) return Cursor is
   begin
      if Position.Container = null then
         return No_Element;
      end if;

      if Position.Index > Index_Type'First then
         return (Position.Container, Index_Type'Pred (Position.Index));
      end if;

      return No_Element;
   end Previous;

   -------------------
   -- Query_Element --
   -------------------

   procedure Query_Element
     (Container : Vector;
      Index     : Index_Type;
      Process   : not null access procedure (Element : in Element_Type))
   is
      subtype T is Index_Type'Base range
        Index_Type'First .. Container.Last;

      E : Element_Type renames Container.Elements (T'(Index)).all;

      V : Vector renames Container'Unrestricted_Access.all;
      B : Natural renames V.Busy;
      L : Natural renames V.Lock;

   begin
      B := B + 1;
      L := L + 1;

      begin
         Process (E);
      exception
         when others =>
            L := L - 1;
            B := B - 1;
            raise;
      end;

      L := L - 1;
      B := B - 1;
   end Query_Element;

   procedure Query_Element
     (Position : Cursor;
      Process  : not null access procedure (Element : in Element_Type))
   is
   begin
      Query_Element (Position.Container.all, Position.Index, Process);
   end Query_Element;

   ----------
   -- Read --
   ----------

   procedure Read
     (Stream    : access Root_Stream_Type'Class;
      Container : out Vector)
   is
      Length : Count_Type'Base;
      Last   : Index_Type'Base := Index_Type'Pred (Index_Type'First);

      B : Boolean;

   begin
      Clear (Container);

      Count_Type'Base'Read (Stream, Length);

      if Length > Capacity (Container) then
         Reserve_Capacity (Container, Capacity => Length);
      end if;

      for J in Count_Type range 1 .. Length loop
         Last := Index_Type'Succ (Last);

         Boolean'Read (Stream, B);

         if B then
            Container.Elements (Last) :=
              new Element_Type'(Element_Type'Input (Stream));
         end if;

         Container.Last := Last;
      end loop;
   end Read;

   ---------------------
   -- Replace_Element --
   ---------------------

   procedure Replace_Element
     (Container : Vector;
      Index     : Index_Type;
      By        : Element_Type)
   is
      subtype T is Index_Type'Base range
        Index_Type'First .. Container.Last;

      X : Element_Access := Container.Elements (T'(Index));

   begin
      if Container.Lock > 0 then
         raise Program_Error;
      end if;

      Container.Elements (T'(Index)) := new Element_Type'(By);
      Free (X);
   end Replace_Element;

   procedure Replace_Element (Position : Cursor; By : Element_Type) is
   begin
      Replace_Element (Position.Container.all, Position.Index, By);
   end Replace_Element;

   ----------------------
   -- Reserve_Capacity --
   ----------------------

   procedure Reserve_Capacity
     (Container : in out Vector;
      Capacity  : Count_Type)
   is
      N : constant Count_Type := Length (Container);

   begin
      if Capacity = 0 then
         if N = 0 then
            declare
               X : Elements_Access := Container.Elements;
            begin
               Container.Elements := null;
               Free (X);
            end;

         elsif N < Container.Elements'Length then
            if Container.Busy > 0 then
               raise Program_Error;
            end if;

            declare
               subtype Array_Index_Subtype is Index_Type'Base range
                 Index_Type'First .. Container.Last;

               Src : Elements_Type renames
                       Container.Elements (Array_Index_Subtype);

               subtype Array_Subtype is
                 Elements_Type (Array_Index_Subtype);

               X : Elements_Access := Container.Elements;
            begin
               Container.Elements := new Array_Subtype'(Src);
               Free (X);
            end;

         end if;

         return;
      end if;

      if Container.Elements = null then
         declare
            Last_As_Int : constant Int'Base :=
                            Int (Index_Type'First) + Int (Capacity) - 1;

            Last : constant Index_Type :=
                     Index_Type (Last_As_Int);

            subtype Array_Subtype is
              Elements_Type (Index_Type'First .. Last);

         begin
            Container.Elements := new Array_Subtype;
         end;

         return;
      end if;

      if Capacity <= N then
         if N < Container.Elements'Length then
            if Container.Busy > 0 then
               raise Program_Error;
            end if;

            declare
               subtype Array_Index_Subtype is Index_Type'Base range
                 Index_Type'First .. Container.Last;

               Src : Elements_Type renames
                       Container.Elements (Array_Index_Subtype);

               subtype Array_Subtype is
                 Elements_Type (Array_Index_Subtype);

               X : Elements_Access := Container.Elements;

            begin
               Container.Elements := new Array_Subtype'(Src);
               Free (X);
            end;

         end if;

         return;
      end if;

      if Capacity = Container.Elements'Length then
         return;
      end if;

      if Container.Busy > 0 then
         raise Program_Error;
      end if;

      declare
         Last_As_Int : constant Int'Base :=
                         Int (Index_Type'First) + Int (Capacity) - 1;

         Last        : constant Index_Type := Index_Type (Last_As_Int);

         subtype Array_Subtype is
           Elements_Type (Index_Type'First .. Last);

         X : Elements_Access := Container.Elements;

      begin
         Container.Elements := new Array_Subtype;

         declare
            Src : Elements_Type renames
                    X (Index_Type'First .. Container.Last);

            Tgt : Elements_Type renames
                    Container.Elements (Index_Type'First .. Container.Last);

         begin
            Tgt := Src;
         end;

         Free (X);
      end;
   end Reserve_Capacity;

   ------------------
   -- Reverse_Find --
   ------------------

   function Reverse_Find
     (Container : Vector;
      Item      : Element_Type;
      Position  : Cursor := No_Element) return Cursor
   is
      Last : Index_Type'Base;

   begin
      if Position.Container /= null
        and then Position.Container /=
                   Vector_Access'(Container'Unchecked_Access)
      then
         raise Program_Error;
      end if;

      if Position.Container = null
        or else Position.Index > Container.Last
      then
         Last := Container.Last;
      else
         Last := Position.Index;
      end if;

      for Indx in reverse Index_Type'First .. Last loop
         if Container.Elements (Indx) /= null
           and then Container.Elements (Indx).all = Item
         then
            return (Container'Unchecked_Access, Indx);
         end if;
      end loop;

      return No_Element;
   end Reverse_Find;

   ------------------------
   -- Reverse_Find_Index --
   ------------------------

   function Reverse_Find_Index
     (Container : Vector;
      Item      : Element_Type;
      Index     : Index_Type := Index_Type'Last) return Extended_Index
   is
      Last : Index_Type'Base;

   begin
      if Index > Container.Last then
         Last := Container.Last;
      else
         Last := Index;
      end if;

      for Indx in reverse Index_Type'First .. Last loop
         if Container.Elements (Indx) /= null
           and then Container.Elements (Indx).all = Item
         then
            return Indx;
         end if;
      end loop;

      return No_Index;
   end Reverse_Find_Index;

   ---------------------
   -- Reverse_Iterate --
   ---------------------

   procedure Reverse_Iterate
     (Container : Vector;
      Process   : not null access procedure (Position : in Cursor))
   is
      V : Vector renames Container'Unrestricted_Access.all;
      B : Natural renames V.Busy;

   begin
      B := B + 1;

      begin
         for Indx in reverse Index_Type'First .. Container.Last loop
            Process (Cursor'(Container'Unchecked_Access, Indx));
         end loop;
      exception
         when others =>
            B := B - 1;
            raise;
      end;

      B := B - 1;
   end Reverse_Iterate;

   ----------------
   -- Set_Length --
   ----------------

   procedure Set_Length
     (Container : in out Vector;
      Length    : Count_Type)
   is
      N : constant Count_Type := Indefinite_Vectors.Length (Container);

   begin
      if Length = N then
         return;
      end if;

      if Length = 0 then
         Clear (Container);
         return;
      end if;

      if Container.Busy > 0 then
         raise Program_Error;
      end if;

      declare
         Last_As_Int : constant Int'Base :=
                         Int (Index_Type'First) + Int (Length) - 1;

         Last        : constant Index_Type :=
                         Index_Type (Last_As_Int);

      begin
         if Length > N then
            if Length > Capacity (Container) then
               Reserve_Capacity (Container, Capacity => Length);
            end if;

            Container.Last := Last;
            return;
         end if;

         for Indx in reverse Index_Type'Succ (Last) .. Container.Last loop
            declare
               X : Element_Access := Container.Elements (Indx);

            begin
               Container.Elements (Indx) := null;
               Container.Last := Index_Type'Pred (Container.Last);
               Free (X);
            end;
         end loop;
      end;
   end Set_Length;

   ----------
   -- Swap --
   ----------

   procedure Swap
     (Container : Vector;
      I, J      : Index_Type)
   is
      subtype T is Index_Type'Base range
        Index_Type'First .. Container.Last;

      EI : Element_Type renames Container.Elements (T'(I)).all;
      EJ : Element_Type renames Container.Elements (T'(J)).all;

   begin
      if Container.Lock > 0 then
         raise Program_Error;
      end if;

      declare
         EI_Copy : constant Element_Type := EI;
      begin
         EI := EJ;
         EJ := EI_Copy;
      end;
   end Swap;

   procedure Swap (I, J : Cursor)
   is
   begin
      if I.Container = null
        or else J.Container = null
      then
         raise Constraint_Error;
      end if;

      if I.Container /= J.Container then
         raise Program_Error;
      end if;

      Swap (I.Container.all, I.Index, J.Index);
   end Swap;

   ---------------
   -- To_Cursor --
   ---------------

   function To_Cursor
     (Container : Vector;
      Index     : Extended_Index) return Cursor
   is
   begin
      if Index not in Index_Type'First .. Container.Last then
         return No_Element;
      end if;

      return Cursor'(Container'Unchecked_Access, Index);
   end To_Cursor;

   --------------
   -- To_Index --
   --------------

   function To_Index (Position : Cursor) return Extended_Index is
   begin
      if Position.Container = null then
         return No_Index;
      end if;

      if Position.Index <= Position.Container.Last then
         return Position.Index;
      end if;

      return No_Index;
   end To_Index;

   ---------------
   -- To_Vector --
   ---------------

   function To_Vector (Length : Count_Type) return Vector is
   begin
      if Length = 0 then
         return Empty_Vector;
      end if;

      declare
         First       : constant Int := Int (Index_Type'First);
         Last_As_Int : constant Int'Base := First + Int (Length) - 1;
         Last        : constant Index_Type := Index_Type (Last_As_Int);
         Elements    : constant Elements_Access :=
                         new Elements_Type (Index_Type'First .. Last);
      begin
         return (Controlled with Elements, Last, 0, 0);
      end;
   end To_Vector;

   function To_Vector
     (New_Item : Element_Type;
      Length   : Count_Type) return Vector
   is
   begin
      if Length = 0 then
         return Empty_Vector;
      end if;

      declare
         First       : constant Int := Int (Index_Type'First);
         Last_As_Int : constant Int'Base := First + Int (Length) - 1;
         Last        : constant Index_Type := Index_Type (Last_As_Int);
         Elements    : Elements_Access :=
                         new Elements_Type (Index_Type'First .. Last);
      begin
         for Indx in Elements'Range loop
            begin
               Elements (Indx) := new Element_Type'(New_Item);
            exception
               when others =>
                  for J in Index_Type'First .. Index_Type'Pred (Indx) loop
                     Free (Elements (J));
                  end loop;

                  Free (Elements);
                  raise;
            end;

         end loop;

         return (Controlled with Elements, Last, 0, 0);
      end;
   end To_Vector;

   --------------------
   -- Update_Element --
   --------------------

   procedure Update_Element
     (Container : Vector;
      Index     : Index_Type;
      Process   : not null access procedure (Element : in out Element_Type))
   is
      subtype T is Index_Type'Base range
        Index_Type'First .. Container.Last;

      E : Element_Type renames Container.Elements (T'(Index)).all;

      V : Vector renames Container'Unrestricted_Access.all;
      B : Natural renames V.Busy;
      L : Natural renames V.Lock;

   begin
      B := B + 1;
      L := L + 1;

      begin
         Process (E);
      exception
         when others =>
            L := L - 1;
            B := B - 1;
            raise;
      end;

      L := L - 1;
      B := B - 1;
   end Update_Element;

   procedure Update_Element
     (Position : Cursor;
      Process  : not null access procedure (Element : in out Element_Type))
   is
   begin
      Update_Element (Position.Container.all, Position.Index, Process);
   end Update_Element;

   -----------
   -- Write --
   -----------

   procedure Write
     (Stream    : access Root_Stream_Type'Class;
      Container : Vector)
   is
      N : constant Count_Type := Length (Container);

   begin
      Count_Type'Base'Write (Stream, N);

      if N = 0 then
         return;
      end if;

      declare
         E : Elements_Type renames Container.Elements.all;

      begin
         for Indx in Index_Type'First .. Container.Last loop

            --  There's another way to do this.  Instead a separate
            --  Boolean for each element, you could write a Boolean
            --  followed by a count of how many nulls or non-nulls
            --  follow in the array.  Alternately you could use a
            --  signed integer, and use the sign as the indicator
            --  of null-ness.

            if E (Indx) = null then
               Boolean'Write (Stream, False);
            else
               Boolean'Write (Stream, True);
               Element_Type'Output (Stream, E (Indx).all);
            end if;
         end loop;
      end;
   end Write;

end Ada.Containers.Indefinite_Vectors;

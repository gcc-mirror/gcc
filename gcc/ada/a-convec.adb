------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                          ADA.CONTAINERS.VECTORS                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 2004 Free Software Foundation, Inc.            --
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
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- This unit was originally developed by Matthew J Heaney.                  --
------------------------------------------------------------------------------

with Ada.Containers.Generic_Array_Sort;
with Ada.Unchecked_Deallocation;

with System; use type System.Address;

package body Ada.Containers.Vectors is

   type Int is range System.Min_Int .. System.Max_Int;

   procedure Free is
     new Ada.Unchecked_Deallocation (Elements_Type, Elements_Access);

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

            Elements : constant Elements_Access :=
                         new Elements_Type'(RE);

         begin
            return (Controlled with Elements, Right.Last);
         end;
      end if;

      if RN = 0 then
         declare
            LE : Elements_Type renames
                   Left.Elements (Index_Type'First .. Left.Last);

            Elements : constant Elements_Access :=
                         new Elements_Type'(LE);

         begin
            return (Controlled with Elements, Left.Last);
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

         Elements : constant Elements_Access :=
                         new Elements_Type'(LE & RE);

      begin
         return (Controlled with Elements, Last);
      end;
   end "&";

   function "&" (Left  : Vector; Right : Element_Type) return Vector is
      LN : constant Count_Type := Length (Left);

   begin
      if LN = 0 then
         declare
            subtype Elements_Subtype is
              Elements_Type (Index_Type'First .. Index_Type'First);

            Elements : constant Elements_Access :=
                         new Elements_Subtype'(others => Right);

         begin
            return (Controlled with Elements, Index_Type'First);
         end;
      end if;

      declare
         Last_As_Int : constant Int'Base :=
                         Int (Index_Type'First) + Int (LN);

         Last : constant Index_Type := Index_Type (Last_As_Int);

         LE : Elements_Type renames
                Left.Elements (Index_Type'First .. Left.Last);

         subtype ET is Elements_Type (Index_Type'First .. Last);

         Elements : constant Elements_Access := new ET'(LE & Right);

      begin
         return (Controlled with Elements, Last);
      end;
   end "&";

   function "&" (Left  : Element_Type; Right : Vector) return Vector is
      RN : constant Count_Type := Length (Right);

   begin
      if RN = 0 then
         declare
            subtype Elements_Subtype is
              Elements_Type (Index_Type'First .. Index_Type'First);

            Elements : constant Elements_Access :=
                         new Elements_Subtype'(others => Left);

         begin
            return (Controlled with Elements, Index_Type'First);
         end;
      end if;

      declare
         Last_As_Int : constant Int'Base :=
                         Int (Index_Type'First) + Int (RN);

         Last : constant Index_Type := Index_Type (Last_As_Int);

         RE : Elements_Type renames
                Right.Elements (Index_Type'First .. Right.Last);

         subtype ET is Elements_Type (Index_Type'First .. Last);

         Elements : constant Elements_Access := new ET'(Left & RE);

      begin
         return (Controlled with Elements, Last);
      end;
   end "&";

   function "&" (Left, Right  : Element_Type) return Vector is
      subtype IT is Index_Type'Base range
        Index_Type'First .. Index_Type'Succ (Index_Type'First);

      subtype ET is Elements_Type (IT);

      Elements : constant Elements_Access := new ET'(Left, Right);

   begin
      return Vector'(Controlled with Elements, Elements'Last);
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

      for J in Index_Type range Index_Type'First .. Left.Last loop
         if Left.Elements (J) /= Right.Elements (J) then
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
         X : constant Elements_Access := Container.Elements;
         L : constant Index_Type'Base := Container.Last;
         E : Elements_Type renames X (Index_Type'First .. L);
      begin
         Container.Elements := null;
         Container.Last := Index_Type'Pred (Index_Type'First);
         Container.Elements := new Elements_Type'(E);
         Container.Last := L;
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

      Target.Elements (Index_Type'First .. Source.Last) :=
        Source.Elements (Index_Type'First .. Source.Last);

      Target.Last := Source.Last;
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
      Container.Last := Index_Type'Pred (Index_Type'First);
   end Clear;

   --------------
   -- Contains --
   --------------

   function Contains
     (Container : Vector;
      Item      : Element_Type) return Boolean
   is
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
      if Count = 0 then
         return;
      end if;

      declare
         subtype I_Subtype is Index_Type'Base range
           Index_Type'First .. Container.Last;

         I : constant I_Subtype := Index;
         --  TODO: not sure whether to relax this check ???

         I_As_Int : constant Int := Int (I);

         Old_Last_As_Int : constant Int := Index_Type'Pos (Container.Last);

         Count1 : constant Int'Base := Count_Type'Pos (Count);
         Count2 : constant Int'Base := Old_Last_As_Int - I_As_Int + 1;

         N : constant Int'Base := Int'Min (Count1, Count2);

         J_As_Int : constant Int'Base := I_As_Int + N;
         J        : constant Index_Type'Base := Index_Type'Base (J_As_Int);

         E : Elements_Type renames Container.Elements.all;

         New_Last_As_Int : constant Int'Base := Old_Last_As_Int - N;

         New_Last : constant Extended_Index :=
                      Extended_Index (New_Last_As_Int);

      begin
         E (I .. New_Last) := E (J .. Container.Last);
         Container.Last := New_Last;
      end;
   end Delete;

   procedure Delete
     (Container : in out Vector;
      Position  : in out Cursor;
      Count     : Count_Type := 1)
   is
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
         Position := No_Element;
         return;
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
      return Container.Elements (T'(Index));
   end Element;

   function Element (Position : Cursor) return Element_Type is
   begin
      return Element (Position.Container.all, Position.Index);
   end Element;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Container : in out Vector) is
      X : Elements_Access := Container.Elements;
   begin
      Container.Elements := null;
      Container.Last := Index_Type'Pred (Index_Type'First);
      Free (X);
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
        and then Position.Container /=
                   Vector_Access'(Container'Unchecked_Access)
      then
         raise Program_Error;
      end if;

      for J in Position.Index .. Container.Last loop
         if Container.Elements (J) = Item then
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
         if Container.Elements (Indx) = Item then
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

   ------------------
   -- Generic_Sort --
   ------------------

   procedure Generic_Sort (Container : Vector)
   is
      procedure Sort is
         new Generic_Array_Sort
          (Index_Type   => Index_Type,
           Element_Type => Element_Type,
           Array_Type   => Elements_Type,
           "<"          => "<");

   begin
      if Container.Elements = null then
         return;
      end if;

      Sort (Container.Elements (Index_Type'First .. Container.Last));
   end Generic_Sort;

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
      Old_Last : constant Extended_Index := Container.Last;

      Old_Last_As_Int : constant Int := Index_Type'Pos (Old_Last);

      N : constant Int := Count_Type'Pos (Count);

      New_Last_As_Int : constant Int'Base := Old_Last_As_Int + N;

      New_Last : constant Extended_Index := Extended_Index (New_Last_As_Int);

      Index : Index_Type;

      Dst_Last : Index_Type;
      Dst      : Elements_Access;

   begin
      if Count = 0 then
         return;
      end if;

      declare
         subtype Before_Subtype is Index_Type'Base range
           Index_Type'First .. Index_Type'Succ (Container.Last);

         Old_First : constant Before_Subtype := Before;

         Old_First_As_Int : constant Int := Index_Type'Pos (Old_First);

         New_First_As_Int : constant Int'Base := Old_First_As_Int + N;

      begin
         Index := Index_Type (New_First_As_Int);
      end;

      if Container.Elements = null then
         declare
            subtype Elements_Subtype is
              Elements_Type (Index_Type'First .. New_Last);
         begin
            Container.Elements := new Elements_Subtype'(others => New_Item);
         end;

         Container.Last := New_Last;
         return;
      end if;

      if New_Last <= Container.Elements'Last then
         declare
            E : Elements_Type renames Container.Elements.all;
         begin
            E (Index .. New_Last) := E (Before .. Container.Last);
            E (Before .. Index_Type'Pred (Index)) := (others => New_Item);
         end;

         Container.Last := New_Last;
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

         Dst (Before .. Index_Type'Pred (Index)) :=
           (others => New_Item);

         Dst (Index .. New_Last) :=
           Src (Before .. Container.Last);

      exception
         when others =>
            Free (Dst);
            raise;
      end;

      declare
         X : Elements_Access := Container.Elements;
      begin
         Container.Elements := Dst;
         Container.Last := New_Last;
         Free (X);
      end;
   end Insert;

   procedure Insert
     (Container : in out Vector;
      Before    : Extended_Index;
      New_Item  : Vector)
   is
      N : constant Count_Type := Length (New_Item);

   begin
      if N = 0 then
         return;
      end if;

      Insert_Space (Container, Before, Count => N);

      declare
         Dst_Last_As_Int : constant Int'Base :=
                             Int'Base (Before) + Int'Base (N) - 1;

         Dst_Last : constant Index_Type := Index_Type (Dst_Last_As_Int);

      begin
         if Container'Address = New_Item'Address then
            declare
               subtype Src_Index_Subtype is Index_Type'Base range
                 Index_Type'First .. Index_Type'Pred (Before);

               Src : Elements_Type renames
                       Container.Elements (Src_Index_Subtype);

               Index_As_Int : constant Int'Base :=
                                Int (Before) + Src'Length - 1;

               Index : constant Index_Type'Base :=
                         Index_Type'Base (Index_As_Int);

               Dst : Elements_Type renames
                       Container.Elements (Before .. Index);

            begin
               Dst := Src;
            end;

            declare
               subtype Src_Index_Subtype is Index_Type'Base range
                 Index_Type'Succ (Dst_Last) .. Container.Last;

               Src : Elements_Type renames
                       Container.Elements (Src_Index_Subtype);

               Index_As_Int : constant Int'Base :=
                                Dst_Last_As_Int - Src'Length + 1;

               Index : constant Index_Type'Base :=
                         Index_Type'Base (Index_As_Int);

               Dst : Elements_Type renames
                       Container.Elements (Index .. Dst_Last);

            begin
               Dst := Src;
            end;

         else
            Container.Elements (Before .. Dst_Last) :=
              New_Item.Elements (Index_Type'First .. New_Item.Last);
         end if;
      end;
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

      Position := Cursor'(Container'Unchecked_Access, Index);
   end Insert;

   ------------------
   -- Insert_Space --
   ------------------

   procedure Insert_Space
     (Container : in out Vector;
      Before    : Extended_Index;
      Count     : Count_Type := 1)
   is
      Old_Last : constant Extended_Index := Container.Last;

      Old_Last_As_Int : constant Int := Index_Type'Pos (Old_Last);

      N : constant Int := Count_Type'Pos (Count);

      New_Last_As_Int : constant Int'Base := Old_Last_As_Int + N;

      New_Last : constant Extended_Index := Extended_Index (New_Last_As_Int);

      Index : Index_Type;

      Dst_Last : Index_Type;
      Dst      : Elements_Access;

   begin
      if Count = 0 then
         return;
      end if;

      declare
         subtype Before_Subtype is Index_Type'Base range
           Index_Type'First .. Index_Type'Succ (Container.Last);

         Old_First : constant Before_Subtype := Before;

         Old_First_As_Int : constant Int := Index_Type'Pos (Old_First);

         New_First_As_Int : constant Int'Base := Old_First_As_Int + N;

      begin
         Index := Index_Type (New_First_As_Int);
      end;

      if Container.Elements = null then
         Container.Elements :=
           new Elements_Type (Index_Type'First .. New_Last);

         Container.Last := New_Last;
         return;
      end if;

      if New_Last <= Container.Elements'Last then
         declare
            E : Elements_Type renames Container.Elements.all;
         begin
            E (Index .. New_Last) := E (Before .. Container.Last);
         end;

         Container.Last := New_Last;
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

         Dst (Index .. New_Last) :=
           Src (Before .. Container.Last);

      exception
         when others =>
            Free (Dst);
            raise;
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
      Process   : not null access procedure (Position : Cursor))
   is
   begin
      for Indx in Index_Type'First .. Container.Last loop
         Process (Cursor'(Container'Unchecked_Access, Indx));
      end loop;
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
      X : Elements_Access := Target.Elements;

   begin
      if Target'Address = Source'Address then
         return;
      end if;

      if Target.Last >= Index_Type'First then
         raise Constraint_Error;
      end if;

      Target.Elements := null;
      Free (X);

      Target.Elements := Source.Elements;
      Target.Last := Source.Last;

      Source.Elements := null;
      Source.Last := Index_Type'Pred (Index_Type'First);
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
      Process   : not null access procedure (Element : Element_Type))
   is
      subtype T is Index_Type'Base range
        Index_Type'First .. Container.Last;
   begin
      Process (Container.Elements (T'(Index)));
   end Query_Element;

   procedure Query_Element
     (Position : Cursor;
      Process  : not null access procedure (Element : Element_Type))
   is
      Container : Vector renames Position.Container.all;

      subtype T is Index_Type'Base range
        Index_Type'First .. Container.Last;

   begin
      Process (Container.Elements (T'(Position.Index)));
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

   begin
      Clear (Container);

      Count_Type'Base'Read (Stream, Length);

      if Length > Capacity (Container) then
         Reserve_Capacity (Container, Capacity => Length);
      end if;

      for J in Count_Type range 1 .. Length loop
         Last := Index_Type'Succ (Last);
         Element_Type'Read (Stream, Container.Elements (Last));
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
   begin
      Container.Elements (T'(Index)) := By;
   end Replace_Element;

   procedure Replace_Element (Position : Cursor; By : Element_Type) is
      subtype T is Index_Type'Base range
        Index_Type'First .. Position.Container.Last;
   begin
      Position.Container.Elements (T'(Position.Index)) := By;
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

            Last : constant Index_Type := Index_Type (Last_As_Int);

            subtype Array_Subtype is
              Elements_Type (Index_Type'First .. Last);

         begin
            Container.Elements := new Array_Subtype;
         end;

         return;
      end if;

      if Capacity <= N then
         if N < Container.Elements'Length then
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

      declare
         Last_As_Int : constant Int'Base :=
                         Int (Index_Type'First) + Int (Capacity) - 1;

         Last : constant Index_Type := Index_Type (Last_As_Int);

         subtype Array_Subtype is
           Elements_Type (Index_Type'First .. Last);

         E : Elements_Access := new Array_Subtype;

      begin
         declare
            Src : Elements_Type renames
                    Container.Elements (Index_Type'First .. Container.Last);

            Tgt : Elements_Type renames
                    E (Index_Type'First .. Container.Last);

         begin
            Tgt := Src;

         exception
            when others =>
               Free (E);
               raise;
         end;

         declare
            X : Elements_Access := Container.Elements;
         begin
            Container.Elements := E;
            Free (X);
         end;
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
         if Container.Elements (Indx) = Item then
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
         if Container.Elements (Indx) = Item then
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
      Process   : not null access procedure (Position : Cursor))
   is
   begin
      for Indx in reverse Index_Type'First .. Container.Last loop
         Process (Cursor'(Container'Unchecked_Access, Indx));
      end loop;
   end Reverse_Iterate;

   ----------------
   -- Set_Length --
   ----------------

   procedure Set_Length (Container : in out Vector; Length : Count_Type) is
   begin
      if Length = 0 then
         Clear (Container);
         return;
      end if;

      declare
         Last_As_Int : constant Int'Base :=
                         Int (Index_Type'First) + Int (Length) - 1;

         Last        : constant Index_Type := Index_Type (Last_As_Int);

      begin
         if Length > Capacity (Container) then
            Reserve_Capacity (Container, Capacity => Length);
         end if;

         Container.Last := Last;
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

      EI : constant Element_Type := Container.Elements (T'(I));

   begin

      Container.Elements (T'(I)) := Container.Elements (T'(J));
      Container.Elements (T'(J)) := EI;

   end Swap;

   procedure Swap (I, J : Cursor) is

      --  NOTE: The behavior has been liberalized here to
      --  allow I and J to designate different containers.
      --  TODO: Probably this is supposed to raise P_E ???

      subtype TI is Index_Type'Base range
        Index_Type'First .. I.Container.Last;

      EI : Element_Type renames I.Container.Elements (TI'(I.Index));

      EI_Copy : constant Element_Type := EI;

      subtype TJ is Index_Type'Base range
        Index_Type'First .. J.Container.Last;

      EJ : Element_Type renames J.Container.Elements (TJ'(J.Index));

   begin
      EI := EJ;
      EJ := EI_Copy;
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
         return (Controlled with Elements, Last);
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
         Elements    : constant Elements_Access :=
                         new Elements_Type'
                                   (Index_Type'First .. Last => New_Item);
      begin
         return (Controlled with Elements, Last);
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
   begin
      Process (Container.Elements (T'(Index)));
   end Update_Element;

   procedure Update_Element
     (Position : Cursor;
      Process  : not null access procedure (Element : in out Element_Type))
   is
      subtype T is Index_Type'Base range
        Index_Type'First .. Position.Container.Last;
   begin
      Process (Position.Container.Elements (T'(Position.Index)));
   end Update_Element;

   -----------
   -- Write --
   -----------

   procedure Write
     (Stream    : access Root_Stream_Type'Class;
      Container : Vector)
   is
   begin
      Count_Type'Base'Write (Stream, Length (Container));

      for J in Index_Type'First .. Container.Last loop
         Element_Type'Write (Stream, Container.Elements (J));
      end loop;
   end Write;

end Ada.Containers.Vectors;


------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--         A D A . C O N T A I N E R S . F O R M A L _ V E C T O R S        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2010-2013, Free Software Foundation, Inc.         --
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

with Ada.Containers.Generic_Array_Sort;
with System; use type System.Address;

package body Ada.Containers.Formal_Vectors is

   type Int is range System.Min_Int .. System.Max_Int;
   type UInt is mod System.Max_Binary_Modulus;

   function Get_Element
     (Container : Vector;
      Position  : Count_Type) return Element_Type;

   procedure Insert_Space
     (Container : in out Vector;
      Before    : Extended_Index;
      Count     : Count_Type := 1);

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
            E : constant Elements_Array (1 .. Length (Right)) :=
              Right.Elements (1 .. RN);
         begin
            return (Length (Right), E, Last => Right.Last, others => <>);
         end;
      end if;

      if RN = 0 then
         declare
            E : constant Elements_Array (1 .. Length (Left)) :=
              Left.Elements (1 .. LN);
         begin
            return (Length (Left), E, Last => Left.Last, others => <>);
         end;
      end if;

      declare
         N           : constant Int'Base := Int (LN) + Int (RN);
         Last_As_Int : Int'Base;

      begin
         if Int (No_Index) > Int'Last - N then
            raise Constraint_Error with "new length is out of range";
         end if;

         Last_As_Int := Int (No_Index) + N;

         if Last_As_Int > Int (Index_Type'Last) then
            raise Constraint_Error with "new length is out of range";
         end if;

         --  TODO: should check whether length > max capacity (cnt_t'last)  ???

         declare
            Last : constant Index_Type := Index_Type (Last_As_Int);

            LE : constant Elements_Array (1 .. LN) := Left.Elements (1 .. LN);
            RE : Elements_Array renames Right.Elements (1 .. RN);

            Capacity : constant Count_Type := Length (Left) + Length (Right);

         begin
            return (Capacity, LE & RE, Last => Last, others => <>);
         end;
      end;
   end "&";

   function "&" (Left  : Vector; Right : Element_Type) return Vector is
      LN          : constant Count_Type := Length (Left);
      Last_As_Int : Int'Base;

   begin
      if LN = 0 then
         return (1, (1 .. 1 => Right), Index_Type'First, others => <>);
      end if;

      if Int (Index_Type'First) > Int'Last - Int (LN) then
         raise Constraint_Error with "new length is out of range";
      end if;

      Last_As_Int := Int (Index_Type'First) + Int (LN);

      if Last_As_Int > Int (Index_Type'Last) then
         raise Constraint_Error with "new length is out of range";
      end if;

      declare
         Last : constant Index_Type := Index_Type (Last_As_Int);
         LE   : constant Elements_Array (1 .. LN) := Left.Elements (1 .. LN);

         Capacity : constant Count_Type := Length (Left) + 1;

      begin
         return (Capacity, LE & Right, Last => Last, others => <>);
      end;
   end "&";

   function "&" (Left  : Element_Type; Right : Vector) return Vector is
      RN          : constant Count_Type := Length (Right);
      Last_As_Int : Int'Base;

   begin
      if RN = 0 then
         return (1, (1 .. 1 => Left),
                 Index_Type'First, others => <>);
      end if;

      if Int (Index_Type'First) > Int'Last - Int (RN) then
         raise Constraint_Error with "new length is out of range";
      end if;

      Last_As_Int := Int (Index_Type'First) + Int (RN);

      if Last_As_Int > Int (Index_Type'Last) then
         raise Constraint_Error with "new length is out of range";
      end if;

      declare
         Last     : constant Index_Type := Index_Type (Last_As_Int);
         RE       : Elements_Array renames Right.Elements (1 .. RN);
         Capacity : constant Count_Type := 1 + Length (Right);
      begin
         return (Capacity, Left & RE, Last => Last, others => <>);
      end;
   end "&";

   function "&" (Left, Right : Element_Type) return Vector is
   begin
      if Index_Type'First >= Index_Type'Last then
         raise Constraint_Error with "new length is out of range";
      end if;

      declare
         Last : constant Index_Type := Index_Type'First + 1;
      begin
         return (2, (Left, Right), Last => Last, others => <>);
      end;
   end "&";

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Vector) return Boolean is
   begin
      if Left'Address = Right'Address then
         return True;
      end if;

      if Length (Left) /= Length (Right) then
         return False;
      end if;

      for J in Count_Type range 1 .. Length (Left) loop
         if Get_Element (Left, J) /= Get_Element (Right, J) then
            return False;
         end if;
      end loop;

      return True;
   end "=";

   ------------
   -- Append --
   ------------

   procedure Append (Container : in out Vector; New_Item : Vector) is
   begin
      if Is_Empty (New_Item) then
         return;
      end if;

      if Container.Last = Index_Type'Last then
         raise Constraint_Error with "vector is already at its maximum length";
      end if;

      Insert (Container, Container.Last + 1, New_Item);
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

      if Container.Last = Index_Type'Last then
         raise Constraint_Error with "vector is already at its maximum length";
      end if;

      --  TODO: should check whether length > max capacity (cnt_t'last)  ???

      Insert (Container, Container.Last + 1, New_Item, Count);
   end Append;

   ------------
   -- Assign --
   ------------

   procedure Assign (Target : in out Vector; Source : Vector) is
      LS : constant Count_Type := Length (Source);

   begin
      if Target'Address = Source'Address then
         return;
      end if;

      if Target.Capacity < LS then
         raise Constraint_Error;
      end if;

      Clear (Target);

      Target.Elements (1 .. LS) := Source.Elements (1 .. LS);
      Target.Last := Source.Last;
   end Assign;

   --------------
   -- Capacity --
   --------------

   function Capacity (Container : Vector) return Count_Type is
   begin
      return Container.Elements'Length;
   end Capacity;

   -----------
   -- Clear --
   -----------

   procedure Clear (Container : in out Vector) is
   begin
      Container.Last := No_Index;
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

   ----------
   -- Copy --
   ----------

   function Copy
     (Source   : Vector;
      Capacity : Count_Type := 0) return Vector
   is
      LS : constant Count_Type := Length (Source);
      C  : Count_Type;

   begin
      if Capacity = 0 then
         C := LS;
      elsif Capacity >= LS and then Capacity in Capacity_Range then
         C := Capacity;
      else
         raise Capacity_Error;
      end if;

      return Target : Vector (C) do
         Target.Elements (1 .. LS) := Source.Elements (1 .. LS);
         Target.Last := Source.Last;
      end return;
   end Copy;

   ---------------------
   -- Current_To_Last --
   ---------------------

   function Current_To_Last
     (Container : Vector;
      Current : Cursor) return Vector is
      C : Vector (Container.Capacity) := Copy (Container, Container.Capacity);

   begin
      if Current = No_Element then
         Clear (C);
         return C;
      end if;

      if not Has_Element (Container, Current) then
         raise Constraint_Error;
      end if;

      while C.Last /= Container.Last - Current.Index + 1 loop
         Delete_First (C);
      end loop;

      return C;
   end Current_To_Last;

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
         raise Constraint_Error with "Index is out of range (too small)";
      end if;

      if Index > Container.Last then
         if Index > Container.Last + 1 then
            raise Constraint_Error with "Index is out of range (too large)";
         end if;

         return;
      end if;

      if Count = 0 then
         return;
      end if;

      declare
         I_As_Int        : constant Int := Int (Index);
         Old_Last_As_Int : constant Int := Index_Type'Pos (Container.Last);

         Count1 : constant Int'Base := Count_Type'Pos (Count);
         Count2 : constant Int'Base := Old_Last_As_Int - I_As_Int + 1;
         N      : constant Int'Base := Int'Min (Count1, Count2);

         J_As_Int : constant Int'Base := I_As_Int + N;

      begin
         if J_As_Int > Old_Last_As_Int then
            Container.Last := Index - 1;

         else
            declare
               EA : Elements_Array renames Container.Elements;

               II : constant Int'Base := I_As_Int - Int (No_Index);
               I  : constant Count_Type := Count_Type (II);

               JJ : constant Int'Base := J_As_Int - Int (No_Index);
               J  : constant Count_Type := Count_Type (JJ);

               New_Last_As_Int : constant Int'Base := Old_Last_As_Int - N;
               New_Last        : constant Index_Type :=
                 Index_Type (New_Last_As_Int);

               KK : constant Int := New_Last_As_Int - Int (No_Index);
               K  : constant Count_Type := Count_Type (KK);

            begin
               EA (I .. K) := EA (J .. Length (Container));
               Container.Last := New_Last;
            end;
         end if;
      end;
   end Delete;

   procedure Delete
     (Container : in out Vector;
      Position  : in out Cursor;
      Count     : Count_Type := 1)
   is
   begin
      if not Position.Valid then
         raise Constraint_Error with "Position cursor has no element";
      end if;

      if Position.Index > Container.Last then
         raise Program_Error with "Position index is out of range";
      end if;

      Delete (Container, Position.Index, Count);
      Position := No_Element;
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

      Index := Int'Base (Container.Last) - Int'Base (Count);

      if Index < Index_Type'Pos (Index_Type'First) then
         Container.Last := No_Index;
      else
         Container.Last := Index_Type (Index);
      end if;
   end Delete_Last;

   -------------
   -- Element --
   -------------

   function Element
     (Container : Vector;
      Index     : Index_Type) return Element_Type
   is
   begin
      if Index > Container.Last then
         raise Constraint_Error with "Index is out of range";
      end if;

      declare
         II : constant Int'Base := Int (Index) - Int (No_Index);
         I  : constant Count_Type := Count_Type (II);
      begin
         return Get_Element (Container, I);
      end;
   end Element;

   function Element
     (Container : Vector;
      Position  : Cursor) return Element_Type
   is
      Lst : constant Index_Type := Last_Index (Container);

   begin
      if not Position.Valid then
         raise Constraint_Error with "Position cursor has no element";
      end if;

      if Position.Index > Lst then
         raise Constraint_Error with "Position cursor is out of range";
      end if;

      declare
         II : constant Int'Base := Int (Position.Index) - Int (No_Index);
         I  : constant Count_Type := Count_Type (II);
      begin
         return Get_Element (Container, I);
      end;
   end Element;

   ----------
   -- Find --
   ----------

   function Find
     (Container : Vector;
      Item      : Element_Type;
      Position  : Cursor := No_Element) return Cursor
   is
      K    : Count_Type;
      Last : constant Index_Type := Last_Index (Container);

   begin
      if Position.Valid then
         if Position.Index > Last_Index (Container) then
            raise Program_Error with "Position index is out of range";
         end if;
      end if;

      K := Count_Type (Int (Position.Index) - Int (No_Index));

      for J in Position.Index .. Last loop
         if Get_Element (Container, K) = Item then
            return Cursor'(Index => J, others => <>);
         end if;

         K := K + 1;
      end loop;

      return No_Element;
   end Find;

   ----------------
   -- Find_Index --
   ----------------

   function Find_Index
     (Container : Vector;
      Item      : Element_Type;
      Index     : Index_Type := Index_Type'First) return Extended_Index
   is
      K    : Count_Type;
      Last : constant Index_Type := Last_Index (Container);

   begin
      K := Count_Type (Int (Index) - Int (No_Index));
      for Indx in Index .. Last loop
         if Get_Element (Container, K) = Item then
            return Indx;
         end if;

         K := K + 1;
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

      return (True, Index_Type'First);
   end First;

   -------------------
   -- First_Element --
   -------------------

   function First_Element (Container : Vector) return Element_Type is
   begin
      if Is_Empty (Container) then
         raise Constraint_Error with "Container is empty";
      end if;

      return Get_Element (Container, 1);
   end First_Element;

   -----------------
   -- First_Index --
   -----------------

   function First_Index (Container : Vector) return Index_Type is
      pragma Unreferenced (Container);
   begin
      return Index_Type'First;
   end First_Index;

   -----------------------
   -- First_To_Previous --
   -----------------------

   function First_To_Previous
     (Container : Vector;
      Current : Cursor) return Vector is
      C : Vector (Container.Capacity) := Copy (Container, Container.Capacity);

   begin
      if Current = No_Element then
         return C;
      end if;

      if not Has_Element (Container, Current) then
         raise Constraint_Error;
      end if;

      while C.Last /= Current.Index - 1 loop
         Delete_Last (C);
      end loop;
      return C;
   end First_To_Previous;

   ---------------------
   -- Generic_Sorting --
   ---------------------

   package body Generic_Sorting is

      ---------------
      -- Is_Sorted --
      ---------------

      function Is_Sorted (Container : Vector) return Boolean is
         Last : constant Index_Type := Last_Index (Container);

      begin
         if Container.Last <= Last then
            return True;
         end if;

         declare
            L : constant Count_Type := Length (Container);
         begin
            for J in Count_Type range 1 .. L - 1 loop
               if Get_Element (Container, J + 1) <
                  Get_Element (Container, J)
               then
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
      begin
         declare
            TA : Elements_Array renames Target.Elements;
            SA : Elements_Array renames Source.Elements;

            I, J : Count_Type;

         begin
            --  ???
            --           if Target.Last < Index_Type'First then
            --              Move (Target => Target, Source => Source);
            --              return;
            --           end if;

            if Target'Address = Source'Address then
               return;
            end if;

            if Source.Last < Index_Type'First then
               return;
            end if;

            --  I think we're missing this check in a-convec.adb...  ???

            I := Length (Target);
            Set_Length (Target, I + Length (Source));

            J := Length (Target);
            while not Is_Empty (Source) loop
               pragma Assert (Length (Source) <= 1
                 or else not (SA (Length (Source)) <
                     SA (Length (Source) - 1)));

               if I = 0 then
                  TA (1 .. J) := SA (1 .. Length (Source));
                  Source.Last := No_Index;
                  return;
               end if;

               pragma Assert (I <= 1 or else not (TA (I) < TA (I - 1)));

               if SA (Length (Source)) < TA (I) then
                  TA (J) := TA (I);
                  I := I - 1;

               else
                  TA (J) := SA (Length (Source));
                  Source.Last := Source.Last - 1;
               end if;

               J := J - 1;
            end loop;
         end;
      end Merge;

      ----------
      -- Sort --
      ----------

      procedure Sort (Container : in out Vector)
      is
         procedure Sort is
           new Generic_Array_Sort
             (Index_Type   => Count_Type,
              Element_Type => Element_Type,
              Array_Type   => Elements_Array,
              "<"          => "<");

      begin
         if Container.Last <= Index_Type'First then
            return;
         end if;

         Sort (Container.Elements (1 .. Length (Container)));
      end Sort;

   end Generic_Sorting;

   -----------------
   -- Get_Element --
   -----------------

   function Get_Element
     (Container : Vector;
      Position  : Count_Type) return Element_Type
   is
   begin
      return Container.Elements (Position);
   end Get_Element;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element
     (Container : Vector;
      Position  : Cursor) return Boolean
   is
   begin
      if not Position.Valid then
         return False;
      else
         return Position.Index <= Last_Index (Container);
      end if;
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
      N : constant Int := Count_Type'Pos (Count);

      First           : constant Int := Int (Index_Type'First);
      New_Last_As_Int : Int'Base;
      New_Last        : Index_Type;
      New_Length      : UInt;
      Max_Length      : constant UInt := UInt (Container.Capacity);

   begin
      if Before < Index_Type'First then
         raise Constraint_Error with
           "Before index is out of range (too small)";
      end if;

      if Before > Container.Last
        and then Before > Container.Last + 1
      then
         raise Constraint_Error with
           "Before index is out of range (too large)";
      end if;

      if Count = 0 then
         return;
      end if;

      declare
         Old_Last_As_Int : constant Int := Int (Container.Last);

      begin
         if Old_Last_As_Int > Int'Last - N then
            raise Constraint_Error with "new length is out of range";
         end if;

         New_Last_As_Int := Old_Last_As_Int + N;

         if New_Last_As_Int > Int (Index_Type'Last) then
            raise Constraint_Error with "new length is out of range";
         end if;

         New_Length := UInt (New_Last_As_Int - First + Int'(1));

         if New_Length > Max_Length then
            raise Constraint_Error with "new length is out of range";
         end if;

         New_Last := Index_Type (New_Last_As_Int);

         --  Resolve issue of capacity vs. max index  ???
      end;

      declare
         EA : Elements_Array renames Container.Elements;

         BB : constant Int'Base := Int (Before) - Int (No_Index);
         B  : constant Count_Type := Count_Type (BB);

         LL : constant Int'Base := New_Last_As_Int - Int (No_Index);
         L  : constant Count_Type := Count_Type (LL);

      begin
         if Before <= Container.Last then
            declare
               II : constant Int'Base := BB + N;
               I  : constant Count_Type := Count_Type (II);
            begin
               EA (I .. L) := EA (B .. Length (Container));
               EA (B .. I - 1) := (others => New_Item);
            end;

         else
            EA (B .. L) := (others => New_Item);
         end if;
      end;

      Container.Last := New_Last;
   end Insert;

   procedure Insert
     (Container : in out Vector;
      Before    : Extended_Index;
      New_Item  : Vector)
   is
      N : constant Count_Type := Length (New_Item);

   begin
      if Before < Index_Type'First then
         raise Constraint_Error with
           "Before index is out of range (too small)";
      end if;

      if Before > Container.Last
        and then Before > Container.Last + 1
      then
         raise Constraint_Error with
           "Before index is out of range (too large)";
      end if;

      if N = 0 then
         return;
      end if;

      Insert_Space (Container, Before, Count => N);

      declare
         Dst_Last_As_Int : constant Int'Base :=
           Int (Before) + Int (N) - 1 - Int (No_Index);

         Dst_Last : constant Count_Type := Count_Type (Dst_Last_As_Int);

         BB : constant Int'Base := Int (Before) - Int (No_Index);
         B  : constant Count_Type := Count_Type (BB);

      begin
         if Container'Address /= New_Item'Address then
            Container.Elements (B .. Dst_Last) := New_Item.Elements (1 .. N);
            return;
         end if;

         declare
            Src : Elements_Array renames Container.Elements (1 .. B - 1);

            Index_As_Int : constant Int'Base := BB + Src'Length - 1;

            Index : constant Count_Type := Count_Type (Index_As_Int);

            Dst : Elements_Array renames Container.Elements (B .. Index);

         begin
            Dst := Src;
         end;

         if Dst_Last = Length (Container) then
            return;
         end if;

         declare
            Src : Elements_Array renames
                    Container.Elements (Dst_Last + 1 .. Length (Container));

            Index_As_Int : constant Int'Base :=
              Dst_Last_As_Int - Src'Length + 1;

            Index : constant Count_Type := Count_Type (Index_As_Int);

            Dst : Elements_Array renames
                    Container.Elements (Index .. Dst_Last);

         begin
            Dst := Src;
         end;
      end;
   end Insert;

   procedure Insert
     (Container : in out Vector;
      Before    : Cursor;
      New_Item  : Vector)
   is
      Index : Index_Type'Base;

   begin
      if Is_Empty (New_Item) then
         return;
      end if;

      if not Before.Valid
        or else Before.Index > Container.Last
      then
         if Container.Last = Index_Type'Last then
            raise Constraint_Error with
              "vector is already at its maximum length";
         end if;

         Index := Container.Last + 1;

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
      if Is_Empty (New_Item) then
         if not Before.Valid
           or else Before.Index > Container.Last
         then
            Position := No_Element;
         else
            Position := (True, Before.Index);
         end if;

         return;
      end if;

      if not Before.Valid
        or else Before.Index > Container.Last
      then
         if Container.Last = Index_Type'Last then
            raise Constraint_Error with
              "vector is already at its maximum length";
         end if;

         Index := Container.Last + 1;

      else
         Index := Before.Index;
      end if;

      Insert (Container, Index, New_Item);

      Position := Cursor'(True, Index);
   end Insert;

   procedure Insert
     (Container : in out Vector;
      Before    : Cursor;
      New_Item  : Element_Type;
      Count     : Count_Type := 1)
   is
      Index : Index_Type'Base;

   begin
      if Count = 0 then
         return;
      end if;

      if not Before.Valid
        or else Before.Index > Container.Last
      then
         if Container.Last = Index_Type'Last then
            raise Constraint_Error with
              "vector is already at its maximum length";
         end if;

         Index := Container.Last + 1;

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
      if Count = 0 then
         if not Before.Valid
           or else Before.Index > Container.Last
         then
            Position := No_Element;
         else
            Position := (True, Before.Index);
         end if;

         return;
      end if;

      if not Before.Valid
        or else Before.Index > Container.Last
      then
         if Container.Last = Index_Type'Last then
            raise Constraint_Error with
              "vector is already at its maximum length";
         end if;

         Index := Container.Last + 1;

      else
         Index := Before.Index;
      end if;

      Insert (Container, Index, New_Item, Count);

      Position := Cursor'(True, Index);
   end Insert;

   ------------------
   -- Insert_Space --
   ------------------

   procedure Insert_Space
     (Container : in out Vector;
      Before    : Extended_Index;
      Count     : Count_Type := 1)
   is
      N : constant Int := Count_Type'Pos (Count);

      First           : constant Int := Int (Index_Type'First);
      New_Last_As_Int : Int'Base;
      New_Last        : Index_Type;
      New_Length      : UInt;
      Max_Length      : constant UInt := UInt (Count_Type'Last);

   begin
      if Before < Index_Type'First then
         raise Constraint_Error with
           "Before index is out of range (too small)";
      end if;

      if Before > Container.Last
        and then Before > Container.Last + 1
      then
         raise Constraint_Error with
           "Before index is out of range (too large)";
      end if;

      if Count = 0 then
         return;
      end if;

      declare
         Old_Last_As_Int : constant Int := Int (Container.Last);

      begin
         if Old_Last_As_Int > Int'Last - N then
            raise Constraint_Error with "new length is out of range";
         end if;

         New_Last_As_Int := Old_Last_As_Int + N;

         if New_Last_As_Int > Int (Index_Type'Last) then
            raise Constraint_Error with "new length is out of range";
         end if;

         New_Length := UInt (New_Last_As_Int - First + Int'(1));

         if New_Length > Max_Length then
            raise Constraint_Error with "new length is out of range";
         end if;

         New_Last := Index_Type (New_Last_As_Int);

         --  Resolve issue of capacity vs. max index  ???
      end;

      declare
         EA : Elements_Array renames Container.Elements;

         BB : constant Int'Base := Int (Before) - Int (No_Index);
         B  : constant Count_Type := Count_Type (BB);

         LL : constant Int'Base := New_Last_As_Int - Int (No_Index);
         L  : constant Count_Type := Count_Type (LL);

      begin
         if Before <= Container.Last then
            declare
               II : constant Int'Base := BB + N;
               I  : constant Count_Type := Count_Type (II);
            begin
               EA (I .. L) := EA (B .. Length (Container));
            end;
         end if;
      end;

      Container.Last := New_Last;
   end Insert_Space;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Container : Vector) return Boolean is
   begin
      return Last_Index (Container) < Index_Type'First;
   end Is_Empty;

   ----------
   -- Last --
   ----------

   function Last (Container : Vector) return Cursor is
   begin
      if Is_Empty (Container) then
         return No_Element;
      end if;

      return (True, Last_Index (Container));
   end Last;

   ------------------
   -- Last_Element --
   ------------------

   function Last_Element (Container : Vector) return Element_Type is
   begin
      if Is_Empty (Container) then
         raise Constraint_Error with "Container is empty";
      end if;

      return Get_Element (Container, Length (Container));
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
      L : constant Int := Int (Last_Index (Container));
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
      N : constant Count_Type := Length (Source);

   begin
      if Target'Address = Source'Address then
         return;
      end if;

      if N > Target.Capacity then
         raise Constraint_Error with  -- correct exception here???
           "length of Source is greater than capacity of Target";
      end if;

      --  We could also write this as a loop, and incrementally
      --  copy elements from source to target.

      Target.Last := No_Index;  -- in case array assignment files
      Target.Elements (1 .. N) := Source.Elements (1 .. N);

      Target.Last := Source.Last;
      Source.Last := No_Index;
   end Move;

   ----------
   -- Next --
   ----------

   function Next (Container : Vector; Position : Cursor) return Cursor is
   begin
      if not Position.Valid then
         return No_Element;
      end if;

      if Position.Index < Last_Index (Container) then
         return (True, Position.Index + 1);
      end if;

      return No_Element;
   end Next;

   ----------
   -- Next --
   ----------

   procedure Next (Container : Vector; Position : in out Cursor) is
   begin
      if not Position.Valid then
         return;
      end if;

      if Position.Index < Last_Index (Container) then
         Position.Index := Position.Index + 1;
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

   procedure Previous (Container : Vector; Position : in out Cursor) is
   begin
      if not Position.Valid then
         return;
      end if;

      if Position.Index > Index_Type'First
        and then Position.Index <= Last_Index (Container)
      then
         Position.Index := Position.Index - 1;
      else
         Position := No_Element;
      end if;
   end Previous;

   function Previous (Container : Vector; Position : Cursor) return Cursor is
   begin
      if not Position.Valid then
         return No_Element;
      end if;

      if Position.Index > Index_Type'First
        and then Position.Index <= Last_Index (Container)
      then
         return (True, Position.Index - 1);
      end if;

      return No_Element;
   end Previous;

   ---------------------
   -- Replace_Element --
   ---------------------

   procedure Replace_Element
     (Container : in out Vector;
      Index     : Index_Type;
      New_Item  : Element_Type)
   is
   begin
      if Index > Container.Last then
         raise Constraint_Error with "Index is out of range";
      end if;

      declare
         II : constant Int'Base := Int (Index) - Int (No_Index);
         I  : constant Count_Type := Count_Type (II);

      begin
         Container.Elements (I) := New_Item;
      end;
   end Replace_Element;

   procedure Replace_Element
     (Container : in out Vector;
      Position  : Cursor;
      New_Item  : Element_Type)
   is
   begin
      if not Position.Valid then
         raise Constraint_Error with "Position cursor has no element";
      end if;

      if Position.Index > Container.Last then
         raise Constraint_Error with "Position cursor is out of range";
      end if;

      declare
         II : constant Int'Base := Int (Position.Index) - Int (No_Index);
         I  : constant Count_Type := Count_Type (II);
      begin
         Container.Elements (I) := New_Item;
      end;
   end Replace_Element;

   ----------------------
   -- Reserve_Capacity --
   ----------------------

   procedure Reserve_Capacity
     (Container : in out Vector;
      Capacity  : Count_Type)
   is
   begin
      if Capacity > Container.Capacity then
         raise Constraint_Error with "Capacity is out of range";
      end if;
   end Reserve_Capacity;

   ----------------------
   -- Reverse_Elements --
   ----------------------

   procedure Reverse_Elements (Container : in out Vector) is
   begin
      if Length (Container) <= 1 then
         return;
      end if;

      declare
         I, J : Count_Type;
         E    : Elements_Array renames Container.Elements;

      begin
         I := 1;
         J := Length (Container);
         while I < J loop
            declare
               EI : constant Element_Type := E (I);
            begin
               E (I) := E (J);
               E (J) := EI;
            end;

            I := I + 1;
            J := J - 1;
         end loop;
      end;
   end Reverse_Elements;

   ------------------
   -- Reverse_Find --
   ------------------

   function Reverse_Find
     (Container : Vector;
      Item      : Element_Type;
      Position  : Cursor := No_Element) return Cursor
   is
      Last : Index_Type'Base;
      K    : Count_Type;

   begin
      if not Position.Valid
        or else Position.Index > Last_Index (Container)
      then
         Last := Last_Index (Container);
      else
         Last := Position.Index;
      end if;

      K := Count_Type (Int (Last) - Int (No_Index));
      for Indx in reverse Index_Type'First .. Last loop
         if Get_Element (Container, K) = Item then
            return (True, Indx);
         end if;

         K := K - 1;
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
      K    : Count_Type;

   begin
      if Index > Last_Index (Container) then
         Last := Last_Index (Container);
      else
         Last := Index;
      end if;

      K := Count_Type (Int (Last) - Int (No_Index));
      for Indx in reverse Index_Type'First .. Last loop
         if Get_Element (Container, K) = Item then
            return Indx;
         end if;

         K := K - 1;
      end loop;

      return No_Index;
   end Reverse_Find_Index;

   ----------------
   -- Set_Length --
   ----------------

   procedure Set_Length
     (Container : in out Vector;
      New_Length    : Count_Type)
   is
   begin
      if New_Length = Formal_Vectors.Length (Container) then
         return;
      end if;

      if New_Length > Container.Capacity then
         raise Constraint_Error;  -- ???
      end if;

      declare
         Last_As_Int : constant Int'Base :=
           Int (Index_Type'First) + Int (New_Length) - 1;
      begin
         Container.Last := Index_Type'Base (Last_As_Int);
      end;
   end Set_Length;

   ------------------
   -- Strict_Equal --
   ------------------

   function Strict_Equal (Left, Right : Vector) return Boolean is
   begin
      --  On bounded vectors, cursors are indexes. As a consequence, two
      --  vectors always have the same cursor at the same position and
      --  Strict_Equal is simply =

      return Left = Right;
   end Strict_Equal;

   ----------
   -- Swap --
   ----------

   procedure Swap (Container : in out Vector; I, J : Index_Type) is
   begin
      if I > Container.Last then
         raise Constraint_Error with "I index is out of range";
      end if;

      if J > Container.Last then
         raise Constraint_Error with "J index is out of range";
      end if;

      if I = J then
         return;
      end if;

      declare
         II : constant Int'Base := Int (I) - Int (No_Index);
         JJ : constant Int'Base := Int (J) - Int (No_Index);

         EI : Element_Type renames Container.Elements (Count_Type (II));
         EJ : Element_Type renames Container.Elements (Count_Type (JJ));

         EI_Copy : constant Element_Type := EI;

      begin
         EI := EJ;
         EJ := EI_Copy;
      end;
   end Swap;

   procedure Swap (Container : in out Vector; I, J : Cursor) is
   begin
      if not I.Valid then
         raise Constraint_Error with "I cursor has no element";
      end if;

      if not J.Valid then
         raise Constraint_Error with "J cursor has no element";
      end if;

      Swap (Container, I.Index, J.Index);
   end Swap;

   ---------------
   -- To_Cursor --
   ---------------

   function To_Cursor
     (Container : Vector;
      Index     : Extended_Index) return Cursor
   is
   begin
      if Index not in Index_Type'First .. Last_Index (Container) then
         return No_Element;
      end if;

      return Cursor'(True, Index);
   end To_Cursor;

   --------------
   -- To_Index --
   --------------

   function To_Index (Position : Cursor) return Extended_Index is
   begin
      if not Position.Valid then
         return No_Index;
      end if;

      return Position.Index;
   end To_Index;

   ---------------
   -- To_Vector --
   ---------------

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
         Last        : Index_Type;

      begin
         if Last_As_Int > Index_Type'Pos (Index_Type'Last) then
            raise Constraint_Error with "Length is out of range";  -- ???
         end if;

         Last := Index_Type (Last_As_Int);

         return (Length, (others => New_Item), Last => Last,
                 others => <>);
      end;
   end To_Vector;

end Ada.Containers.Formal_Vectors;

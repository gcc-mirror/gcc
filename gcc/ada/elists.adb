------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               E L I S T S                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2020, Free Software Foundation, Inc.         --
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

--  WARNING: There is a C version of this package. Any changes to this
--  source file must be properly reflected in the C header a-elists.h.

with Alloc;
with Debug;  use Debug;
with Output; use Output;
with Table;

package body Elists is

   -------------------------------------
   -- Implementation of Element Lists --
   -------------------------------------

   --  Element lists are composed of three types of entities. The element
   --  list header, which references the first and last elements of the
   --  list, the elements themselves which are singly linked and also
   --  reference the nodes on the list, and finally the nodes themselves.
   --  The following diagram shows how an element list is represented:

   --       +----------------------------------------------------+
   --       |  +------------------------------------------+      |
   --       |  |                                          |      |
   --       V  |                                          V      |
   --    +-----|--+    +-------+    +-------+         +-------+  |
   --    |  Elmt  |    |  1st  |    |  2nd  |         |  Last |  |
   --    |  List  |--->|  Elmt |--->|  Elmt  ---...-->|  Elmt ---+
   --    | Header |    |   |   |    |   |   |         |   |   |
   --    +--------+    +---|---+    +---|---+         +---|---+
   --                      |            |                 |
   --                      V            V                 V
   --                  +-------+    +-------+         +-------+
   --                  |       |    |       |         |       |
   --                  | Node1 |    | Node2 |         | Node3 |
   --                  |       |    |       |         |       |
   --                  +-------+    +-------+         +-------+

   --  The list header is an entry in the Elists table. The values used for
   --  the type Elist_Id are subscripts into this table. The First_Elmt field
   --  (Lfield1) points to the first element on the list, or to No_Elmt in the
   --  case of an empty list. Similarly the Last_Elmt field (Lfield2) points to
   --  the last element on the list or to No_Elmt in the case of an empty list.

   --  The elements themselves are entries in the Elmts table. The Next field
   --  of each entry points to the next element, or to the Elist header if this
   --  is the last item in the list. The Node field points to the node which
   --  is referenced by the corresponding list entry.

   -------------------------
   -- Element List Tables --
   -------------------------

   type Elist_Header is record
      First : Elmt_Id;
      Last  : Elmt_Id;
   end record;

   package Elists is new Table.Table (
     Table_Component_Type => Elist_Header,
     Table_Index_Type     => Elist_Id'Base,
     Table_Low_Bound      => First_Elist_Id,
     Table_Initial        => Alloc.Elists_Initial,
     Table_Increment      => Alloc.Elists_Increment,
     Table_Name           => "Elists");

   type Elmt_Item is record
      Node : Node_Or_Entity_Id;
      Next : Union_Id;
   end record;

   package Elmts is new Table.Table (
     Table_Component_Type => Elmt_Item,
     Table_Index_Type     => Elmt_Id'Base,
     Table_Low_Bound      => First_Elmt_Id,
     Table_Initial        => Alloc.Elmts_Initial,
     Table_Increment      => Alloc.Elmts_Increment,
     Table_Name           => "Elmts");

   -----------------
   -- Append_Elmt --
   -----------------

   procedure Append_Elmt (N : Node_Or_Entity_Id; To : Elist_Id) is
      L : constant Elmt_Id := Elists.Table (To).Last;

   begin
      Elmts.Increment_Last;
      Elmts.Table (Elmts.Last).Node := N;
      Elmts.Table (Elmts.Last).Next := Union_Id (To);

      if L = No_Elmt then
         Elists.Table (To).First := Elmts.Last;
      else
         Elmts.Table (L).Next := Union_Id (Elmts.Last);
      end if;

      Elists.Table (To).Last  := Elmts.Last;

      if Debug_Flag_N then
         Write_Str ("Append new element Elmt_Id = ");
         Write_Int (Int (Elmts.Last));
         Write_Str (" to list Elist_Id = ");
         Write_Int (Int (To));
         Write_Str (" referencing Node_Or_Entity_Id = ");
         Write_Int (Int (N));
         Write_Eol;
      end if;
   end Append_Elmt;

   ---------------------
   -- Append_New_Elmt --
   ---------------------

   procedure Append_New_Elmt (N : Node_Or_Entity_Id; To : in out Elist_Id) is
   begin
      if To = No_Elist then
         To := New_Elmt_List;
      end if;

      Append_Elmt (N, To);
   end Append_New_Elmt;

   ------------------------
   -- Append_Unique_Elmt --
   ------------------------

   procedure Append_Unique_Elmt (N : Node_Or_Entity_Id; To : Elist_Id) is
      Elmt : Elmt_Id;
   begin
      Elmt := First_Elmt (To);
      loop
         if No (Elmt) then
            Append_Elmt (N, To);
            return;
         elsif Node (Elmt) = N then
            return;
         else
            Next_Elmt (Elmt);
         end if;
      end loop;
   end Append_Unique_Elmt;

   --------------
   -- Contains --
   --------------

   function Contains (List : Elist_Id; N : Node_Or_Entity_Id) return Boolean is
      Elmt : Elmt_Id;

   begin
      if Present (List) then
         Elmt := First_Elmt (List);
         while Present (Elmt) loop
            if Node (Elmt) = N then
               return True;
            end if;

            Next_Elmt (Elmt);
         end loop;
      end if;

      return False;
   end Contains;

   --------------------
   -- Elists_Address --
   --------------------

   function Elists_Address return System.Address is
   begin
      return Elists.Table (First_Elist_Id)'Address;
   end Elists_Address;

   -------------------
   -- Elmts_Address --
   -------------------

   function Elmts_Address return System.Address is
   begin
      return Elmts.Table (First_Elmt_Id)'Address;
   end Elmts_Address;

   ----------------
   -- First_Elmt --
   ----------------

   function First_Elmt (List : Elist_Id) return Elmt_Id is
   begin
      pragma Assert (List > Elist_Low_Bound);
      return Elists.Table (List).First;
   end First_Elmt;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Elists.Init;
      Elmts.Init;
   end Initialize;

   -----------------------
   -- Insert_Elmt_After --
   -----------------------

   procedure Insert_Elmt_After (N : Node_Or_Entity_Id; Elmt : Elmt_Id) is
      Nxt : constant Union_Id := Elmts.Table (Elmt).Next;

   begin
      pragma Assert (Elmt /= No_Elmt);

      Elmts.Increment_Last;
      Elmts.Table (Elmts.Last).Node := N;
      Elmts.Table (Elmts.Last).Next := Nxt;

      Elmts.Table (Elmt).Next := Union_Id (Elmts.Last);

      if Nxt in Elist_Range then
         Elists.Table (Elist_Id (Nxt)).Last := Elmts.Last;
      end if;
   end Insert_Elmt_After;

   ------------------------
   -- Is_Empty_Elmt_List --
   ------------------------

   function Is_Empty_Elmt_List (List : Elist_Id) return Boolean is
   begin
      return Elists.Table (List).First = No_Elmt;
   end Is_Empty_Elmt_List;

   -------------------
   -- Last_Elist_Id --
   -------------------

   function Last_Elist_Id return Elist_Id is
   begin
      return Elists.Last;
   end Last_Elist_Id;

   ---------------
   -- Last_Elmt --
   ---------------

   function Last_Elmt (List : Elist_Id) return Elmt_Id is
   begin
      return Elists.Table (List).Last;
   end Last_Elmt;

   ------------------
   -- Last_Elmt_Id --
   ------------------

   function Last_Elmt_Id return Elmt_Id is
   begin
      return Elmts.Last;
   end Last_Elmt_Id;

   -----------------
   -- List_Length --
   -----------------

   function List_Length (List : Elist_Id) return Nat is
      Elmt : Elmt_Id;
      N    : Nat;

   begin
      if List = No_Elist then
         return 0;

      else
         N := 0;
         Elmt := First_Elmt (List);
         loop
            if No (Elmt) then
               return N;
            else
               N := N + 1;
               Next_Elmt (Elmt);
            end if;
         end loop;
      end if;
   end List_Length;

   ----------
   -- Lock --
   ----------

   procedure Lock is
   begin
      Elists.Release;
      Elists.Locked := True;
      Elmts.Release;
      Elmts.Locked := True;
   end Lock;

   --------------------
   -- New_Copy_Elist --
   --------------------

   function New_Copy_Elist (List : Elist_Id) return Elist_Id is
      Result : Elist_Id;
      Elmt   : Elmt_Id;

   begin
      if List = No_Elist then
         return No_Elist;

      --  Replicate the contents of the input list while preserving the
      --  original order.

      else
         Result := New_Elmt_List;

         Elmt := First_Elmt (List);
         while Present (Elmt) loop
            Append_Elmt (Node (Elmt), Result);
            Next_Elmt (Elmt);
         end loop;

         return Result;
      end if;
   end New_Copy_Elist;

   -------------------
   -- New_Elmt_List --
   -------------------

   function New_Elmt_List return Elist_Id is
   begin
      Elists.Increment_Last;
      Elists.Table (Elists.Last).First := No_Elmt;
      Elists.Table (Elists.Last).Last  := No_Elmt;

      if Debug_Flag_N then
         Write_Str ("Allocate new element list, returned ID = ");
         Write_Int (Int (Elists.Last));
         Write_Eol;
      end if;

      return Elists.Last;
   end New_Elmt_List;

   ---------------
   -- Next_Elmt --
   ---------------

   function Next_Elmt (Elmt : Elmt_Id) return Elmt_Id is
      N : constant Union_Id := Elmts.Table (Elmt).Next;

   begin
      if N in Elist_Range then
         return No_Elmt;
      else
         return Elmt_Id (N);
      end if;
   end Next_Elmt;

   procedure Next_Elmt (Elmt : in out Elmt_Id) is
   begin
      Elmt := Next_Elmt (Elmt);
   end Next_Elmt;

   --------
   -- No --
   --------

   function No (List : Elist_Id) return Boolean is
   begin
      return List = No_Elist;
   end No;

   function No (Elmt : Elmt_Id) return Boolean is
   begin
      return Elmt = No_Elmt;
   end No;

   ----------
   -- Node --
   ----------

   function Node (Elmt : Elmt_Id) return Node_Or_Entity_Id is
   begin
      if Elmt = No_Elmt then
         return Empty;
      else
         return Elmts.Table (Elmt).Node;
      end if;
   end Node;

   ----------------
   -- Num_Elists --
   ----------------

   function Num_Elists return Nat is
   begin
      return Int (Elmts.Last) - Int (Elmts.First) + 1;
   end Num_Elists;

   ------------------
   -- Prepend_Elmt --
   ------------------

   procedure Prepend_Elmt (N : Node_Or_Entity_Id; To : Elist_Id) is
      F : constant Elmt_Id := Elists.Table (To).First;

   begin
      Elmts.Increment_Last;
      Elmts.Table (Elmts.Last).Node := N;

      if F = No_Elmt then
         Elists.Table (To).Last := Elmts.Last;
         Elmts.Table (Elmts.Last).Next := Union_Id (To);
      else
         Elmts.Table (Elmts.Last).Next := Union_Id (F);
      end if;

      Elists.Table (To).First  := Elmts.Last;
   end Prepend_Elmt;

   -------------------------
   -- Prepend_Unique_Elmt --
   -------------------------

   procedure Prepend_Unique_Elmt (N : Node_Or_Entity_Id; To : Elist_Id) is
   begin
      if not Contains (To, N) then
         Prepend_Elmt (N, To);
      end if;
   end Prepend_Unique_Elmt;

   -------------
   -- Present --
   -------------

   function Present (List : Elist_Id) return Boolean is
   begin
      return List /= No_Elist;
   end Present;

   function Present (Elmt : Elmt_Id) return Boolean is
   begin
      return Elmt /= No_Elmt;
   end Present;

   ------------
   -- Remove --
   ------------

   procedure Remove (List : Elist_Id; N : Node_Or_Entity_Id) is
      Elmt : Elmt_Id;

   begin
      if Present (List) then
         Elmt := First_Elmt (List);
         while Present (Elmt) loop
            if Node (Elmt) = N then
               Remove_Elmt (List, Elmt);
               exit;
            end if;

            Next_Elmt (Elmt);
         end loop;
      end if;
   end Remove;

   -----------------
   -- Remove_Elmt --
   -----------------

   procedure Remove_Elmt (List : Elist_Id; Elmt : Elmt_Id) is
      Nxt : Elmt_Id;
      Prv : Elmt_Id;

   begin
      Nxt := Elists.Table (List).First;

      --  Case of removing only element in the list

      if Elmts.Table (Nxt).Next in Elist_Range then
         pragma Assert (Nxt = Elmt);

         Elists.Table (List).First := No_Elmt;
         Elists.Table (List).Last  := No_Elmt;

      --  Case of removing the first element in the list

      elsif Nxt = Elmt then
         Elists.Table (List).First := Elmt_Id (Elmts.Table (Nxt).Next);

      --  Case of removing second or later element in the list

      else
         loop
            Prv := Nxt;
            Nxt := Elmt_Id (Elmts.Table (Prv).Next);
            exit when Nxt = Elmt
              or else Elmts.Table (Nxt).Next in Elist_Range;
         end loop;

         pragma Assert (Nxt = Elmt);

         Elmts.Table (Prv).Next := Elmts.Table (Nxt).Next;

         if Elmts.Table (Prv).Next in Elist_Range then
            Elists.Table (List).Last := Prv;
         end if;
      end if;
   end Remove_Elmt;

   ----------------------
   -- Remove_Last_Elmt --
   ----------------------

   procedure Remove_Last_Elmt (List : Elist_Id) is
      Nxt : Elmt_Id;
      Prv : Elmt_Id;

   begin
      Nxt := Elists.Table (List).First;

      --  Case of removing only element in the list

      if Elmts.Table (Nxt).Next in Elist_Range then
         Elists.Table (List).First := No_Elmt;
         Elists.Table (List).Last  := No_Elmt;

      --  Case of at least two elements in list

      else
         loop
            Prv := Nxt;
            Nxt := Elmt_Id (Elmts.Table (Prv).Next);
            exit when Elmts.Table (Nxt).Next in Elist_Range;
         end loop;

         Elmts.Table (Prv).Next   := Elmts.Table (Nxt).Next;
         Elists.Table (List).Last := Prv;
      end if;
   end Remove_Last_Elmt;

   ------------------
   -- Replace_Elmt --
   ------------------

   procedure Replace_Elmt (Elmt : Elmt_Id; New_Node : Node_Or_Entity_Id) is
   begin
      Elmts.Table (Elmt).Node := New_Node;
   end Replace_Elmt;

   ------------
   -- Unlock --
   ------------

   procedure Unlock is
   begin
      Elists.Locked := False;
      Elmts.Locked := False;
   end Unlock;

end Elists;

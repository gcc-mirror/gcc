------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               E L I S T S                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2004 Free Software Foundation, Inc.          --
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
     Table_Index_Type     => Elist_Id,
     Table_Low_Bound      => First_Elist_Id,
     Table_Initial        => Alloc.Elists_Initial,
     Table_Increment      => Alloc.Elists_Increment,
     Table_Name           => "Elists");

   type Elmt_Item is record
      Node : Node_Id;
      Next : Union_Id;
   end record;

   package Elmts is new Table.Table (
     Table_Component_Type => Elmt_Item,
     Table_Index_Type     => Elmt_Id,
     Table_Low_Bound      => First_Elmt_Id,
     Table_Initial        => Alloc.Elmts_Initial,
     Table_Increment      => Alloc.Elmts_Increment,
     Table_Name           => "Elmts");

   -----------------
   -- Append_Elmt --
   -----------------

   procedure Append_Elmt (Node : Node_Id; To : Elist_Id) is
      L : constant Elmt_Id := Elists.Table (To).Last;

   begin
      Elmts.Increment_Last;
      Elmts.Table (Elmts.Last).Node := Node;
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
         Write_Str (" referencing Node_Id = ");
         Write_Int (Int (Node));
         Write_Eol;
      end if;
   end Append_Elmt;

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

   procedure Insert_Elmt_After (Node : Node_Id; Elmt : Elmt_Id) is
      N : constant Union_Id := Elmts.Table (Elmt).Next;

   begin

      pragma Assert (Elmt /= No_Elmt);

      Elmts.Increment_Last;
      Elmts.Table (Elmts.Last).Node := Node;
      Elmts.Table (Elmts.Last).Next := N;

      Elmts.Table (Elmt).Next := Union_Id (Elmts.Last);

      if N in Elist_Range then
         Elists.Table (Elist_Id (N)).Last := Elmts.Last;
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

   ----------
   -- Lock --
   ----------

   procedure Lock is
   begin
      Elists.Locked := True;
      Elmts.Locked := True;
      Elists.Release;
      Elmts.Release;
   end Lock;

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

   -----------
   -- Node --
   -----------

   function Node (Elmt : Elmt_Id) return Node_Id is
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

   procedure Prepend_Elmt (Node : Node_Id; To : Elist_Id) is
      F : constant Elmt_Id := Elists.Table (To).First;

   begin
      Elmts.Increment_Last;
      Elmts.Table (Elmts.Last).Node := Node;

      if F = No_Elmt then
         Elists.Table (To).Last := Elmts.Last;
         Elmts.Table (Elmts.Last).Next := Union_Id (To);
      else
         Elmts.Table (Elmts.Last).Next := Union_Id (F);
      end if;

      Elists.Table (To).First  := Elmts.Last;

   end Prepend_Elmt;

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

   procedure Replace_Elmt (Elmt : Elmt_Id; New_Node : Node_Id) is
   begin
      Elmts.Table (Elmt).Node := New_Node;
   end Replace_Elmt;

   ---------------
   -- Tree_Read --
   ---------------

   procedure Tree_Read is
   begin
      Elists.Tree_Read;
      Elmts.Tree_Read;
   end Tree_Read;

   ----------------
   -- Tree_Write --
   ----------------

   procedure Tree_Write is
   begin
      Elists.Tree_Write;
      Elmts.Tree_Write;
   end Tree_Write;

end Elists;

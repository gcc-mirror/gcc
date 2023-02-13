------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                            G N A T . T A B L E                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 1998-2023, AdaCore                     --
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

with System;        use System;
with System.Memory; use System.Memory;

package body GNAT.Table is

   --------------
   -- Allocate --
   --------------

   procedure Allocate (Num : Integer := 1) is
   begin
      Tab.Allocate (The_Instance, Num);
   end Allocate;

   function Allocate (Num : Integer := 1) return Valid_Table_Index_Type is
      Result : constant Valid_Table_Index_Type := Last + 1;
   begin
      Allocate (Num);
      return Result;
   end Allocate;

   ------------
   -- Append --
   ------------

   procedure Append (New_Val : Table_Component_Type) is
   begin
      Tab.Append (The_Instance, New_Val);
   end Append;

   ----------------
   -- Append_All --
   ----------------

   procedure Append_All (New_Vals : Table_Type) is
   begin
      Tab.Append_All (The_Instance, New_Vals);
   end Append_All;

   --------------------
   -- Decrement_Last --
   --------------------

   procedure Decrement_Last is
   begin
      Tab.Decrement_Last (The_Instance);
   end Decrement_Last;

   -----------
   -- First --
   -----------

   function First return Table_Index_Type is
   begin
      return Tab.First;
   end First;

   --------------
   -- For_Each --
   --------------

   procedure For_Each is
      procedure For_Each is new Tab.For_Each (Action);
   begin
      For_Each (The_Instance);
   end For_Each;

   ----------
   -- Free --
   ----------

   procedure Free is
   begin
      Tab.Free (The_Instance);
   end Free;

   --------------------
   -- Increment_Last --
   --------------------

   procedure Increment_Last is
   begin
      Tab.Increment_Last (The_Instance);
   end Increment_Last;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty return Boolean is
   begin
      return Tab.Is_Empty (The_Instance);
   end Is_Empty;

   ----------
   -- Init --
   ----------

   procedure Init is
   begin
      Tab.Init (The_Instance);
   end Init;

   ----------
   -- Last --
   ----------

   function Last return Table_Last_Type is
   begin
      return Tab.Last (The_Instance);
   end Last;

   -------------
   -- Release --
   -------------

   procedure Release is
   begin
      Tab.Release (The_Instance);
   end Release;

   -------------
   -- Restore --
   -------------

   procedure Restore (T : in out Saved_Table) is
   begin
      Init;
      Tab.Move (From => T, To => The_Instance);
   end Restore;

   ----------
   -- Save --
   ----------

   function Save return Saved_Table is
      Result : Saved_Table;
   begin
      Tab.Move (From => The_Instance, To => Result);
      return Result;
   end Save;

   --------------
   -- Set_Item --
   --------------

   procedure Set_Item
     (Index : Valid_Table_Index_Type;
      Item  : Table_Component_Type)
   is
   begin
      Tab.Set_Item (The_Instance, Index, Item);
   end Set_Item;

   --------------
   -- Set_Last --
   --------------

   procedure Set_Last (New_Val : Table_Last_Type) is
   begin
      Tab.Set_Last (The_Instance, New_Val);
   end Set_Last;

   ----------------
   -- Sort_Table --
   ----------------

   procedure Sort_Table is
      procedure Sort_Table is new Tab.Sort_Table (Lt);
   begin
      Sort_Table (The_Instance);
   end Sort_Table;

end GNAT.Table;

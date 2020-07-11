------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                            G N A T . T A B L E                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 1998-2020, AdaCore                     --
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

--  This package provides a singleton version of GNAT.Dynamic_Tables
--  (g-dyntab.ads). See that package for documentation. This package just
--  declares a single instance of GNAT.Dynamic_Tables.Instance, and provides
--  wrappers for all the subprograms, passing that single instance.

--  Note that these three interfaces should remain synchronized to keep as much
--  coherency as possible among these related units:
--
--     GNAT.Dynamic_Tables
--     GNAT.Table
--     Table (the compiler unit)

with GNAT.Dynamic_Tables;

generic
   type Table_Component_Type is private;
   type Table_Index_Type     is range <>;

   Table_Low_Bound   : Table_Index_Type := Table_Index_Type'First;
   Table_Initial     : Positive := 8;
   Table_Increment   : Natural := 100;
   Table_Name        : String := ""; -- for debugging printouts
   pragma Unreferenced (Table_Name);
   Release_Threshold : Natural := 0;

package GNAT.Table is
   pragma Elaborate_Body;

   package Tab is new GNAT.Dynamic_Tables
     (Table_Component_Type,
      Table_Index_Type,
      Table_Low_Bound,
      Table_Initial,
      Table_Increment,
      Release_Threshold);

   subtype Valid_Table_Index_Type is Tab.Valid_Table_Index_Type;
   subtype Table_Last_Type is Tab.Table_Last_Type;
   subtype Table_Type is Tab.Table_Type;
   function "=" (X, Y : Table_Type) return Boolean renames Tab."=";

   subtype Table_Ptr is Tab.Table_Ptr;

   The_Instance : Tab.Instance;
   Table : Table_Ptr renames The_Instance.Table;
   Locked : Boolean renames The_Instance.Locked;

   function Is_Empty return Boolean;

   procedure Init;
   pragma Inline (Init);
   procedure Free;
   pragma Inline (Free);

   function First return Table_Index_Type;
   pragma Inline (First);

   function Last return Table_Last_Type;
   pragma Inline (Last);

   procedure Release;
   pragma Inline (Release);

   procedure Set_Last (New_Val : Table_Last_Type);
   pragma Inline (Set_Last);

   procedure Increment_Last;
   pragma Inline (Increment_Last);

   procedure Decrement_Last;
   pragma Inline (Decrement_Last);

   procedure Append (New_Val : Table_Component_Type);
   pragma Inline (Append);

   procedure Append_All (New_Vals : Table_Type);
   pragma Inline (Append_All);

   procedure Set_Item
     (Index : Valid_Table_Index_Type;
      Item  : Table_Component_Type);
   pragma Inline (Set_Item);

   subtype Saved_Table is Tab.Instance;
   --  Type used for Save/Restore subprograms

   function Save return Saved_Table;
   pragma Inline (Save);
   --  Resets table to empty, but saves old contents of table in returned
   --  value, for possible later restoration by a call to Restore.

   procedure Restore (T : in out Saved_Table);
   pragma Inline (Restore);
   --  Given a Saved_Table value returned by a prior call to Save, restores
   --  the table to the state it was in at the time of the Save call.

   procedure Allocate (Num : Integer := 1);
   function Allocate (Num : Integer := 1) return Valid_Table_Index_Type;
   pragma Inline (Allocate);
   --  Adds Num to Last. The function version also returns the old value of
   --  Last + 1. Note that this function has the possible side effect of
   --  reallocating the table. This means that a reference X.Table (X.Allocate)
   --  is incorrect, since the call to X.Allocate may modify the results of
   --  calling X.Table.

   generic
     with procedure Action
       (Index : Valid_Table_Index_Type;
        Item  : Table_Component_Type;
        Quit  : in out Boolean) is <>;
   procedure For_Each;
   pragma Inline (For_Each);

   generic
     with function Lt (Comp1, Comp2 : Table_Component_Type) return Boolean;
   procedure Sort_Table;
   pragma Inline (Sort_Table);

end GNAT.Table;

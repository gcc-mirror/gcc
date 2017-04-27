------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                T A B L E                                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2017, Free Software Foundation, Inc.         --
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

--  This package is a wrapper for GNAT.Table, for use in the compiler front
--  end. It adds the Tree_Write/Tree_Read functionality; everything else is
--  just a renaming of GNAT.Table. See GNAT.Table (g-table.ads) and
--  GNAT.Dynamic_Tables (g-dyntab.ads) for documentation.

--  Note that these three interfaces should remain synchronized to keep as much
--  coherency as possible among these related units:
--
--     GNAT.Dynamic_Tables
--     GNAT.Table
--     Table (the compiler unit)

with Types; use Types;
with GNAT.Table;

package Table is
   pragma Elaborate_Body;

   generic
      type Table_Component_Type is private;
      type Table_Index_Type     is range <>;

      Table_Low_Bound   : Table_Index_Type := Table_Index_Type'First;
      Table_Initial     : Pos := 8;
      Table_Increment   : Nat := 100;
      Table_Name        : String; -- for debugging printouts
      Release_Threshold : Nat := 0;

   package Table is

      package Tab is new GNAT.Table
        (Table_Component_Type,
         Table_Index_Type,
         Table_Low_Bound,
         Positive (Table_Initial),
         Natural (Table_Increment),
         Table_Name,
         Natural (Release_Threshold));

      subtype Valid_Table_Index_Type is Tab.Valid_Table_Index_Type;
      subtype Table_Last_Type is Tab.Table_Last_Type;
      subtype Table_Type is Tab.Table_Type;

      subtype Table_Ptr is Tab.Table_Ptr;

      Table : Table_Ptr renames Tab.Table;

      Locked : Boolean renames Tab.Locked;

      function Is_Empty return Boolean renames Tab.Is_Empty;

      procedure Init renames Tab.Init;

      function First return Table_Index_Type renames Tab.First;
      function Last return Table_Last_Type renames Tab.Last;

      procedure Release renames Tab.Release;

      procedure Free renames Tab.Free;

      procedure Set_Last (New_Val : Table_Last_Type) renames Tab.Set_Last;

      procedure Increment_Last renames Tab.Increment_Last;
      procedure Decrement_Last renames Tab.Decrement_Last;

      procedure Append (New_Val : Table_Component_Type) renames Tab.Append;
      procedure Append_All (New_Vals : Table_Type) renames Tab.Append_All;

      procedure Set_Item
        (Index : Valid_Table_Index_Type;
         Item  : Table_Component_Type) renames Tab.Set_Item;

      subtype Saved_Table is Tab.Saved_Table;
      function Save return Saved_Table renames Tab.Save;
      procedure Restore (T : in out Saved_Table) renames Tab.Restore;

      procedure Tree_Write;
      --  Writes out contents of table using Tree_IO

      procedure Tree_Read;
      --  Initializes table by reading contents previously written with the
      --  Tree_Write call, also using Tree_IO.

   end Table;
end Table;

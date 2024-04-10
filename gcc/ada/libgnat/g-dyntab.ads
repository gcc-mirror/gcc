------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                   G N A T . D Y N A M I C _ T A B L E S                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2000-2024, AdaCore                     --
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

--  Resizable one dimensional array support

--  This package provides an implementation of dynamically resizable one
--  dimensional arrays. The idea is to mimic the normal Ada semantics for
--  arrays as closely as possible with the one additional capability of
--  dynamically modifying the value of the Last attribute.

--  This package provides a facility similar to that of Ada.Containers.Vectors.

--  Note that these three interfaces should remain synchronized to keep as much
--  coherency as possible among these related units:
--
--     GNAT.Dynamic_Tables
--     GNAT.Table
--     Table (the compiler unit)

--  Note: this unit is used during bootstrap, see ADA_GENERATED_FILES in
--  gcc-interface/Make-lang.in for details on the constraints.

with Ada.Unchecked_Conversion;

generic
   type Table_Component_Type is private;
   type Table_Index_Type     is range <>;

   Table_Low_Bound   : Table_Index_Type := Table_Index_Type'First;
   Table_Initial     : Positive := 8;
   Table_Increment   : Natural := 100;
   Release_Threshold : Natural := 0; -- size in bytes

package GNAT.Dynamic_Tables is

   --  Table_Component_Type and Table_Index_Type specify the type of the array,
   --  Table_Low_Bound is the lower bound. The effect is roughly to declare:

   --    Table : array (Table_Low_Bound .. <>) of Table_Component_Type;

   --  The lower bound of Table_Index_Type is ignored.

   --  Table_Component_Type must not be a type with controlled parts.

   --  The Table_Initial value controls the allocation of the table when it is
   --  first allocated.

   --  The Table_Increment value controls the amount of increase, if the table
   --  has to be increased in size. The value given is a percentage value (e.g.
   --  100 = increase table size by 100%, i.e. double it).

   --  The Last and Set_Last subprograms provide control over the current
   --  logical allocation. They are quite efficient, so they can be used
   --  freely (expensive reallocation occurs only at major granularity
   --  chunks controlled by the allocation parameters).

   --  WARNING: On HPPA, the virtual addressing approach used in this unit is
   --  incompatible with the indexing instructions on the HPPA. So when using
   --  this unit, compile your application with -mdisable-indexing.

   --  WARNING: If the table is reallocated, then the address of all its
   --  components will change. So do not capture the address of an element
   --  and then use the address later after the table may be reallocated. One
   --  tricky case of this is passing an element of the table to a subprogram
   --  by reference where the table gets reallocated during the execution of
   --  the subprogram. The best rule to follow is never to pass a table element
   --  as a parameter except for the case of IN mode parameters with scalar
   --  values.

   pragma Assert (Table_Low_Bound /= Table_Index_Type'Base'First);

   subtype Valid_Table_Index_Type is Table_Index_Type'Base
     range Table_Low_Bound .. Table_Index_Type'Base'Last;
   subtype Table_Last_Type is Table_Index_Type'Base
     range Table_Low_Bound - 1 .. Table_Index_Type'Base'Last;

   --  Table_Component_Type must not be a type with controlled parts.

   --  The Table_Initial value controls the allocation of the table when it is
   --  first allocated.

   --  The Table_Increment value controls the amount of increase, if the table
   --  has to be increased in size. The value given is a percentage value (e.g.
   --  100 = increase table size by 100%, i.e. double it).

   --  The Last and Set_Last subprograms provide control over the current
   --  logical allocation. They are quite efficient, so they can be used
   --  freely (expensive reallocation occurs only at major granularity
   --  chunks controlled by the allocation parameters).

   --  Note: For backward compatibility we do not make the table components
   --  aliased, since for Ada 95 this would have restricted the use of tables
   --  for discriminated types. If it is necessary to take the access of a
   --  table element, use Unrestricted_Access.

   type Table_Type is
     array (Valid_Table_Index_Type range <>) of Table_Component_Type;
   subtype Big_Table_Type is
     Table_Type (Table_Low_Bound .. Valid_Table_Index_Type'Last);
   --  We work with pointers to a bogus array type that is constrained with
   --  the maximum possible range bound. This means that the pointer is a thin
   --  pointer, which is more efficient. Since subscript checks in any case
   --  must be on the logical, rather than physical bounds, safety is not
   --  compromised by this approach.

   --  To get subscript checking, rename a slice of the Table, like this:

   --     Table : Table_Type renames T.Table (First .. Last (T));

   --  and then refer to components of Table.

   type Table_Ptr is access all Big_Table_Type;
   for Table_Ptr'Storage_Size use 0;
   --  The table is actually represented as a pointer to allow reallocation

   type Table_Private is private;
   --  Table private data that is not exported in Instance

   --  Private use only:
   subtype Empty_Table_Array_Type is
     Table_Type (Table_Low_Bound .. Table_Low_Bound - 1);
   type Empty_Table_Array_Ptr is access all Empty_Table_Array_Type;
   Empty_Table_Array : aliased Empty_Table_Array_Type;
   function Empty_Table_Array_Ptr_To_Table_Ptr is
     new Ada.Unchecked_Conversion (Empty_Table_Array_Ptr, Table_Ptr);
   Empty_Table_Ptr : constant Table_Ptr :=
             Empty_Table_Array_Ptr_To_Table_Ptr (Empty_Table_Array'Access);
   --  End private use only. The above are used to initialize Table to point to
   --  an empty array.

   type Instance is record
      Table : Table_Ptr := Empty_Table_Ptr;
      --  The table itself. The lower bound is the value of First. Logically
      --  the upper bound is the current value of Last (although the actual
      --  size of the allocated table may be larger than this). The program may
      --  only access and modify Table entries in the range First .. Last.
      --
      --  It's a good idea to access this via a renaming of a slice, in order
      --  to ensure bounds checking, as in:
      --
      --     Tab : Table_Type renames X.Table (First .. X.Last);
      --
      --  Note: The Table component must come first. See declarations of
      --  SCO_Unit_Table and SCO_Table in scos.h.

      Locked : Boolean := False;
      --  Table reallocation is permitted only if this is False. A client may
      --  set Locked to True, in which case any operation that might expand or
      --  shrink the table will cause an assertion failure. While a table is
      --  locked, its address in memory remains fixed and unchanging.

      P : Table_Private;
   end record;

   function Is_Empty (T : Instance) return Boolean;
   pragma Inline (Is_Empty);

   procedure Init (T : in out Instance);
   --  Reinitializes the table to empty. There is no need to call this before
   --  using a table; tables default to empty.

   procedure Free (T : in out Instance) renames Init;

   function First return Table_Index_Type;
   pragma Inline (First);
   --  Export First as synonym for Table_Low_Bound (parallel with use of Last)

   function Last (T : Instance) return Table_Last_Type;
   pragma Inline (Last);
   --  Returns the current value of the last used entry in the table, which can
   --  then be used as a subscript for Table.

   procedure Release (T : in out Instance);
   --  Storage is allocated in chunks according to the values given in the
   --  Table_Initial and Table_Increment parameters. If Release_Threshold is
   --  0 or the length of the table does not exceed this threshold then a call
   --  to Release releases all storage that is allocated, but is not logically
   --  part of the current array value; otherwise the call to Release leaves
   --  the current array value plus 0.1% of the current table length free
   --  elements located at the end of the table. This parameter facilitates
   --  reopening large tables and adding a few elements without allocating a
   --  chunk of memory. In both cases current array values are not affected by
   --  this call.

   procedure Set_Last (T : in out Instance; New_Val : Table_Last_Type);
   pragma Inline (Set_Last);
   --  This procedure sets Last to the indicated value. If necessary the table
   --  is reallocated to accommodate the new value (i.e. on return the
   --  allocated table has an upper bound of at least Last). If Set_Last
   --  reduces the size of the table, then logically entries are removed from
   --  the table. If Set_Last increases the size of the table, then new entries
   --  are logically added to the table.

   procedure Increment_Last (T : in out Instance);
   pragma Inline (Increment_Last);
   --  Adds 1 to Last (same as Set_Last (Last + 1))

   procedure Decrement_Last (T : in out Instance);
   pragma Inline (Decrement_Last);
   --  Subtracts 1 from Last (same as Set_Last (Last - 1))

   procedure Append (T : in out Instance; New_Val : Table_Component_Type);
   pragma Inline (Append);
   --  Appends New_Val onto the end of the table
   --  Equivalent to:
   --    Increment_Last (T);
   --    T.Table (T.Last) := New_Val;

   procedure Append_All (T : in out Instance; New_Vals : Table_Type);
   --  Appends all components of New_Vals

   procedure Set_Item
     (T     : in out Instance;
      Index : Valid_Table_Index_Type;
      Item  : Table_Component_Type);
   pragma Inline (Set_Item);
   --  Put Item in the table at position Index. If Index points to an existing
   --  item (i.e. it is in the range First .. Last (T)), the item is replaced.
   --  Otherwise (i.e. Index > Last (T)), the table is expanded, and Last is
   --  set to Index.

   procedure Move (From, To : in out Instance);
   --  Moves from From to To, and sets From to empty

   procedure Allocate (T : in out Instance; Num : Integer := 1);
   pragma Inline (Allocate);
   --  Adds Num to Last

   generic
     with procedure Action
       (Index : Valid_Table_Index_Type;
        Item  : Table_Component_Type;
        Quit  : in out Boolean) is <>;
   procedure For_Each (Table : Instance);
   --  Calls procedure Action for each component of the table, or until one of
   --  these calls set Quit to True.

   generic
     with function Lt (Comp1, Comp2 : Table_Component_Type) return Boolean;
   procedure Sort_Table (Table : in out Instance);
   --  This procedure sorts the components of the table into ascending order
   --  making calls to Lt to do required comparisons, and using assignments
   --  to move components around. The Lt function returns True if Comp1 is
   --  less than Comp2 (in the sense of the desired sort), and False if Comp1
   --  is greater than Comp2. For equal objects it does not matter if True or
   --  False is returned (it is slightly more efficient to return False). The
   --  sort is not stable (the order of equal items in the table is not
   --  preserved).

private

   type Table_Private is record
      Last_Allocated : Table_Last_Type := Table_Low_Bound - 1;
      --  Subscript of the maximum entry in the currently allocated table.
      --  Initial value ensures that we initially allocate the table.

      Last : Table_Last_Type := Table_Low_Bound - 1;
      --  Current value of Last function

      --  Invariant: Last <= Last_Allocated
   end record;

end GNAT.Dynamic_Tables;

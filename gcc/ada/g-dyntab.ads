------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                   G N A T . D Y N A M I C _ T A B L E S                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2000-2005 Ada Core Technologies, Inc.           --
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

--  Resizable one dimensional array support

--  This package provides an implementation of dynamically resizable one
--  dimensional arrays. The idea is to mimic the normal Ada semantics for
--  arrays as closely as possible with the one additional capability of
--  dynamically modifying the value of the Last attribute.

--  This package provides a facility similar to that of GNAT.Table, except
--  that this package declares a type that can be used to define dynamic
--  instances of the table, while an instantiation of GNAT.Table creates a
--  single instance of the table type.

--  Note that this interface should remain synchronized with those in
--  GNAT.Table and the GNAT compiler source unit Table to keep as much
--  coherency as possible between these three related units.

generic
   type Table_Component_Type is private;
   type Table_Index_Type     is range <>;

   Table_Low_Bound : Table_Index_Type;
   Table_Initial   : Positive;
   Table_Increment : Natural;

package GNAT.Dynamic_Tables is

   --  Table_Component_Type and Table_Index_Type specify the type of the
   --  array, Table_Low_Bound is the lower bound. Index_type must be an
   --  integer type. The effect is roughly to declare:

   --    Table : array (Table_Low_Bound .. <>) of Table_Component_Type;

   --    Note: since the upper bound can be one less than the lower
   --    bound for an empty array, the table index type must be able
   --    to cover this range, e.g. if the lower bound is 1, then the
   --    Table_Index_Type should be Natural rather than Positive.

   --  Table_Component_Type may be any Ada type, except that controlled
   --  types are not supported. Note however that default initialization
   --  will NOT occur for array components.

   --  The Table_Initial values controls the allocation of the table when
   --  it is first allocated, either by default, or by an explicit Init
   --  call.

   --  The Table_Increment value controls the amount of increase, if the
   --  table has to be increased in size. The value given is a percentage
   --  value (e.g. 100 = increase table size by 100%, i.e. double it).

   --  The Last and Set_Last subprograms provide control over the current
   --  logical allocation. They are quite efficient, so they can be used
   --  freely (expensive reallocation occurs only at major granularity
   --  chunks controlled by the allocation parameters).

   --  Note: we do not make the table components aliased, since this would
   --  restrict the use of table for discriminated types. If it is necessary
   --  to take the access of a table element, use Unrestricted_Access.

   type Table_Type is
     array (Table_Index_Type range <>) of Table_Component_Type;

   subtype Big_Table_Type is
     Table_Type (Table_Low_Bound .. Table_Index_Type'Last);
   --  We work with pointers to a bogus array type that is constrained
   --  with the maximum possible range bound. This means that the pointer
   --  is a thin pointer, which is more efficient. Since subscript checks
   --  in any case must be on the logical, rather than physical bounds,
   --  safety is not compromised by this approach.

   type Table_Ptr is access all Big_Table_Type;
   --  The table is actually represented as a pointer to allow
   --  reallocation.

   type Table_Private is private;
   --  table private data that is not exported in Instance.

   type Instance is record
      Table : aliased Table_Ptr := null;
   --  The table itself. The lower bound is the value of Low_Bound.
   --  Logically the upper bound is the current value of Last (although
   --  the actual size of the allocated table may be larger than this).
   --  The program may only access and modify Table entries in the
   --  range First .. Last.

      P : Table_Private;
   end record;

   procedure Init (T : in out Instance);
   --  This procedure allocates a new table of size Initial (freeing any
   --  previously allocated larger table). Init must be called before using
   --  the table. Init is convenient in reestablishing a table for new use.

   function Last (T : in Instance) return Table_Index_Type;
   pragma Inline (Last);
   --  Returns the current value of the last used entry in the table,
   --  which can then be used as a subscript for Table. Note that the
   --  only way to modify Last is to call the Set_Last procedure. Last
   --  must always be used to determine the logically last entry.

   procedure Release (T : in out Instance);
   --  Storage is allocated in chunks according to the values given in the
   --  Initial and Increment parameters. A call to Release releases all
   --  storage that is allocated, but is not logically part of the current
   --  array value. Current array values are not affected by this call.

   procedure Free (T : in out Instance);
   --  Free all allocated memory for the table. A call to init is required
   --  before any use of this table after calling Free.

   First : constant Table_Index_Type := Table_Low_Bound;
   --  Export First as synonym for Low_Bound (parallel with use of Last)

   procedure Set_Last (T : in out Instance; New_Val : Table_Index_Type);
   pragma Inline (Set_Last);
   --  This procedure sets Last to the indicated value. If necessary the
   --  table is reallocated to accommodate the new value (i.e. on return
   --  the allocated table has an upper bound of at least Last). If
   --  Set_Last reduces the size of the table, then logically entries are
   --  removed from the table. If Set_Last increases the size of the
   --  table, then new entries are logically added to the table.

   procedure Increment_Last (T : in out Instance);
   pragma Inline (Increment_Last);
   --  Adds 1 to Last (same as Set_Last (Last + 1)

   procedure Decrement_Last (T : in out Instance);
   pragma Inline (Decrement_Last);
   --  Subtracts 1 from Last (same as Set_Last (Last - 1)

   procedure Append (T : in out Instance; New_Val : Table_Component_Type);
   pragma Inline (Append);
   --  Equivalent to:
   --    Increment_Last (T);
   --    T.Table (T.Last) := New_Val;
   --  i.e. the table size is increased by one, and the given new item
   --  stored in the newly created table element.

   procedure Set_Item
     (T     : in out Instance;
      Index : Table_Index_Type;
      Item  : Table_Component_Type);
   pragma Inline (Set_Item);
   --  Put Item in the table at position Index. The table is expanded if
   --  current table length is less than Index and in that case Last is set to
   --  Index. Item will replace any value already present in the table at this
   --  position.

   procedure Allocate (T : in out Instance; Num : Integer := 1);
   pragma Inline (Allocate);
   --  Adds Num to Last

   generic
     with procedure Action
       (Index : Table_Index_Type;
        Item  : Table_Component_Type;
        Quit  : in out Boolean) is <>;
   procedure For_Each (Table : Instance);
   --  Calls procedure Action for each component of the table Table, or until
   --  one of these calls set Quit to True.

   generic
     with function Lt (Comp1, Comp2 : Table_Component_Type) return Boolean;
   procedure Sort_Table (Table : in out Instance);
   --  This procedure sorts the components of table Table into ascending
   --  order making calls to Lt to do required comparisons, and using
   --  assignments to move components around. The Lt function returns True
   --  if Comp1 is less than Comp2 (in the sense of the desired sort), and
   --  False if Comp1 is greater than Comp2. For equal objects it does not
   --  matter if True or False is returned (it is slightly more efficient
   --  to return False). The sort is not stable (the order of equal items
   --  in the table is not preserved).

private
   type Table_Private is record
      Max : Integer;
      --  Subscript of the maximum entry in the currently allocated table

      Length : Integer := 0;
      --  Number of entries in currently allocated table. The value of zero
      --  ensures that we initially allocate the table.

      Last_Val : Integer;
      --  Current value of Last
   end record;

end GNAT.Dynamic_Tables;

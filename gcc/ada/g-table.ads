------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                            G N A T . T A B L E                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.1 $
--                                                                          --
--            Copyright (C) 1998-2001 Ada Core Technologies, Inc.           --
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
-- GNAT is maintained by Ada Core Technologies Inc (http://www.gnat.com).   --
--                                                                          --
------------------------------------------------------------------------------

--  Resizable one dimensional array support

--  This package provides an implementation of dynamically resizable one
--  dimensional arrays. The idea is to mimic the normal Ada semantics for
--  arrays as closely as possible with the one additional capability of
--  dynamically modifying the value of the Last attribute.

--  This package provides a facility similar to that of GNAT.Dynamic_Tables,
--  except that this package declares a single instance of the table type,
--  while an instantiation of GNAT.Dynamic_Tables creates a type that can be
--  used to define dynamic instances of the table.

--  Note that this interface should remain synchronized with those in
--  GNAT.Dynamic_Tables and the GNAT compiler source unit Table to keep
--  as much coherency as possible between these three related units.

generic
   type Table_Component_Type is private;
   type Table_Index_Type     is range <>;

   Table_Low_Bound : Table_Index_Type;
   Table_Initial   : Positive;
   Table_Increment : Natural;

package GNAT.Table is
pragma Elaborate_Body (Table);

   --  Table_Component_Type and Table_Index_Type specify the type of the
   --  array, Table_Low_Bound is the lower bound. Index_type must be an
   --  integer type. The effect is roughly to declare:

   --    Table : array (Table_Index_Type range Table_Low_Bound .. <>)
   --                       of Table_Component_Type;

   --    Note: since the upper bound can be one less than the lower
   --    bound for an empty array, the table index type must be able
   --    to cover this range, e.g. if the lower bound is 1, then the
   --    Table_Index_Type should be Natural rather than Positive.

   --  Table_Component_Type may be any Ada type, except that controlled
   --  types are not supported. Note however that default initialization
   --  will NOT occur for array components.

   --  The Table_Initial values controls the allocation of the table when
   --  it is first allocated, either by default, or by an explicit Init call.

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
   --  The table is actually represented as a pointer to allow reallocation

   Table : aliased Table_Ptr := null;
   --  The table itself. The lower bound is the value of Low_Bound.
   --  Logically the upper bound is the current value of Last (although
   --  the actual size of the allocated table may be larger than this).
   --  The program may only access and modify Table entries in the range
   --  First .. Last.

   Locked : Boolean := False;
   --  Table expansion is permitted only if this switch is set to False. A
   --  client may set Locked to True, in which case any attempt to expand
   --  the table will cause an assertion failure. Note that while a table
   --  is locked, its address in memory remains fixed and unchanging.

   procedure Init;
   --  This procedure allocates a new table of size Initial (freeing any
   --  previously allocated larger table). It is not necessary to call
   --  Init when a table is first instantiated (since the instantiation does
   --  the same initialization steps). However, it is harmless to do so, and
   --  Init is convenient in reestablishing a table for new use.

   function Last return Table_Index_Type;
   pragma Inline (Last);
   --  Returns the current value of the last used entry in the table, which
   --  can then be used as a subscript for Table. Note that the only way to
   --  modify Last is to call the Set_Last procedure. Last must always be
   --  used to determine the logically last entry.

   procedure Release;
   --  Storage is allocated in chunks according to the values given in the
   --  Initial and Increment parameters. A call to Release releases all
   --  storage that is allocated, but is not logically part of the current
   --  array value. Current array values are not affected by this call.

   procedure Free;
   --  Free all allocated memory for the table. A call to init is required
   --  before any use of this table after calling Free.

   First : constant Table_Index_Type := Table_Low_Bound;
   --  Export First as synonym for Low_Bound (parallel with use of Last)

   procedure Set_Last (New_Val : Table_Index_Type);
   pragma Inline (Set_Last);
   --  This procedure sets Last to the indicated value. If necessary the
   --  table is reallocated to accommodate the new value (i.e. on return
   --  the allocated table has an upper bound of at least Last). If Set_Last
   --  reduces the size of the table, then logically entries are removed
   --  from the table. If Set_Last increases the size of the table, then
   --  new entries are logically added to the table.

   procedure Increment_Last;
   pragma Inline (Increment_Last);
   --  Adds 1 to Last (same as Set_Last (Last + 1).

   procedure Decrement_Last;
   pragma Inline (Decrement_Last);
   --  Subtracts 1 from Last (same as Set_Last (Last - 1).

   procedure Append (New_Val : Table_Component_Type);
   pragma Inline (Append);
   --  Equivalent to:
   --    x.Increment_Last;
   --    x.Table (x.Last) := New_Val;
   --  i.e. the table size is increased by one, and the given new item
   --  stored in the newly created table element.

   procedure Set_Item
     (Index : Table_Index_Type;
      Item  : Table_Component_Type);
   pragma Inline (Set_Item);
   --  Put Item in the table at position Index. The table is expanded if the
   --  current table length is less than Index and in that case Last is set to
   --  Index. Item will replace any value already present in the table at this
   --  position.

   function Allocate (Num : Integer := 1) return Table_Index_Type;
   pragma Inline (Allocate);
   --  Adds Num to Last, and returns the old value of Last + 1. Note that
   --  this function has the possible side effect of reallocating the table.
   --  This means that a reference X.Table (X.Allocate) is incorrect, since
   --  the call to X.Allocate may modify the results of calling X.Table.

end GNAT.Table;

------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                T A B L E                                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2003 Free Software Foundation, Inc.          --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package provides an implementation of dynamically resizable one
--  dimensional arrays. The idea is to mimic the normal Ada semantics for
--  arrays as closely as possible with the one additional capability of
--  dynamically modifying the value of the Last attribute.

--  Note that this interface should remain synchronized with those in
--  GNAT.Table and GNAT.Dynamic_Tables to keep coherency between these
--  three related units.

with Types; use Types;

package Table is
pragma Elaborate_Body (Table);

   generic
      type Table_Component_Type is private;
      type Table_Index_Type     is range <>;

      Table_Low_Bound  : Table_Index_Type;
      Table_Initial    : Pos;
      Table_Increment  : Nat;
      Table_Name       : String;

   package Table is

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
      --  it is first allocated, either by default, or by an explicit Init
      --  call. The value used is Opt.Table_Factor * Table_Initial.

      --  The Table_Increment value controls the amount of increase, if the
      --  table has to be increased in size. The value given is a percentage
      --  value (e.g. 100 = increase table size by 100%, i.e. double it).

      --  The Table_Name parameter is simply use in debug output messages it
      --  has no other usage, and is not referenced in non-debugging mode.

      --  The Last and Set_Last subprograms provide control over the current
      --  logical allocation. They are quite efficient, so they can be used
      --  freely (expensive reallocation occurs only at major granularity
      --  chunks controlled by the allocation parameters).

      --  Note: We do not make the table components aliased, since this would
      --  restict the use of table for discriminated types. If it is necessary
      --  to take the access of a table element, use Unrestricted_Access.

      --  WARNING: On HPPA, the virtual addressing approach used in this unit
      --  is incompatible with the indexing instructions on the HPPA. So when
      --  using this unit, compile your application with -mdisable-indexing.

      --  WARNING: If the table is reallocated, then the address of all its
      --  components will change. So do not capture the address of an element
      --  and then use the address later after the table may be reallocated.
      --  One tricky case of this is passing an element of the table to a
      --  subprogram by reference where the table gets reallocated during
      --  the execution of the subprogram. The best rule to follow is never
      --  to pass a table element as a parameter except for the case of IN
      --  mode parameters with scalar values.

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
      --  is locked, its address in memory remains fixed and unchanging. This
      --  feature is used to control table expansion during Gigi processing.
      --  Gigi assumes that tables other than the Uint and Ureal tables do
      --  not move during processing, which means that they cannot be expanded.
      --  The Locked flag is used to enforce this restriction.

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
      --  Put Item in the table at position Index. The table is expanded if
      --  current table length is less than Index and in that case Last is set
      --  to Index. Item will replace any value already present in the table
      --  at this position.

      type Saved_Table is private;
      --  Type used for Save/Restore subprograms

      function Save return Saved_Table;
      --  Resets table to empty, but saves old contents of table in returned
      --  value, for possible later restoration by a call to Restore.

      procedure Restore (T : Saved_Table);
      --  Given a Saved_Table value returned by a prior call to Save, restores
      --  the table to the state it was in at the time of the Save call.

      procedure Tree_Write;
      --  Writes out contents of table using Tree_IO

      procedure Tree_Read;
      --  Initializes table by reading contents previously written
      --  with the Tree_Write call (also using Tree_IO)

   private

      Last_Val : Int;
      --  Current value of Last. Note that we declare this in the private part
      --  because we don't want the client to modify Last except through one of
      --  the official interfaces (since a modification to Last may require a
      --  reallocation of the table).

      Max : Int;
      --  Subscript of the maximum entry in the currently allocated table

      type Saved_Table is record
         Last_Val : Int;
         Max      : Int;
         Table    : Table_Ptr;
      end record;

   end Table;
end Table;

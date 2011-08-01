------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                         G N A T . S P I T B O L                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 1997-2010, AdaCore                     --
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

--  SPITBOL-like interface facilities

--  This package provides a set of interfaces to semantic operations copied
--  from SPITBOL, including a complete implementation of SPITBOL pattern
--  matching. The code is derived from the original SPITBOL MINIMAL sources,
--  created by Robert Dewar. The translation is not exact, but the
--  algorithmic approaches are similar.

with Ada.Finalization;      use Ada.Finalization;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces;            use Interfaces;

package GNAT.Spitbol is
   pragma Preelaborate;

   --  The Spitbol package relies heavily on the Unbounded_String package,
   --  using the synonym VString for variable length string. The following
   --  declarations define this type and other useful abbreviations.

   subtype VString is Ada.Strings.Unbounded.Unbounded_String;

   function V (Source : String) return VString
     renames Ada.Strings.Unbounded.To_Unbounded_String;

   function S (Source : VString) return String
     renames Ada.Strings.Unbounded.To_String;

   Nul : VString renames Ada.Strings.Unbounded.Null_Unbounded_String;

   -------------------------
   -- Facilities Provided --
   -------------------------

   --  The SPITBOL support in GNAT consists of this package together with
   --  several child packages. In this package, we have first a set of
   --  useful string functions, copied exactly from the corresponding
   --  SPITBOL functions, except that we had to rename REVERSE because
   --  reverse is a reserved word (it is now Reverse_String).

   --  The second element of the parent package is a generic implementation
   --  of a table facility. In SPITBOL, the TABLE function allows general
   --  mappings from any datatype to any other datatype, and of course, as
   --  always, we can freely mix multiple types in the same table.

   --  The Ada version of tables is strongly typed, so the indexing type and
   --  the range type are always of a consistent type. In this implementation
   --  we only provide VString as an indexing type, since this is by far the
   --  most common case. The generic instantiation specifies the range type
   --  to be used.

   --  Three child packages provide standard instantiations of this table
   --  package for three common datatypes:

   --    GNAT.Spitbol.Table_Boolean     (file g-sptabo.ads)

   --      The range type is Boolean. The default value is False. This
   --      means that this table is essentially a representation of a set.

   --    GNAT.Spitbol.Table_Integer     (file g-sptain.ads)

   --      The range type is Integer. The default value is Integer'First.
   --      This provides a general mapping from strings to integers.

   --    GNAT.Spitbol.Table_VString     (file g-sptavs.ads)

   --      The range type is VString. The default value is the null string.
   --      This provides a general mapping from strings to strings.

   --  Finally there is another child package:

   --    GNAT.Spitbol.Patterns          (file g-spipat.ads)

   --       This child package provides a complete implementation of SPITBOL
   --       pattern matching. The spec contains a complete tutorial on the
   --       use of pattern matching.

   ---------------------------------
   -- Standard String Subprograms --
   ---------------------------------

   --  This section contains some operations on unbounded strings that are
   --  closely related to those in the package Unbounded.Strings, but they
   --  correspond to the SPITBOL semantics for these operations.

   function Char (Num : Natural) return Character;
   pragma Inline (Char);
   --  Equivalent to Character'Val (Num)

   function Lpad
     (Str : VString;
      Len : Natural;
      Pad : Character := ' ') return VString;
   function Lpad
     (Str : String;
      Len : Natural;
      Pad : Character := ' ') return VString;
   --  If the length of Str is greater than or equal to Len, then Str is
   --  returned unchanged. Otherwise, The value returned is obtained by
   --  concatenating Length (Str) - Len instances of the Pad character to
   --  the left hand side.

   procedure Lpad
     (Str  : in out VString;
      Len  : Natural;
      Pad  : Character := ' ');
   --  The procedure form is identical to the function form, except that
   --  the result overwrites the input argument Str.

   function Reverse_String (Str : VString) return VString;
   function Reverse_String (Str : String)  return VString;
   --  Returns result of reversing the string Str, i.e. the result returned
   --  is a mirror image (end-for-end reversal) of the input string.

   procedure Reverse_String (Str : in out VString);
   --  The procedure form is identical to the function form, except that the
   --  result overwrites the input argument Str.

   function Rpad
     (Str : VString;
      Len : Natural;
      Pad : Character := ' ') return VString;
   function Rpad
     (Str : String;
      Len : Natural;
      Pad : Character := ' ') return VString;
   --  If the length of Str is greater than or equal to Len, then Str is
   --  returned unchanged. Otherwise, The value returned is obtained by
   --  concatenating Length (Str) - Len instances of the Pad character to
   --  the right hand side.

   procedure Rpad
     (Str  : in out VString;
      Len  : Natural;
      Pad  : Character := ' ');
   --  The procedure form is identical to the function form, except that the
   --  result overwrites the input argument Str.

   function Size (Source : VString) return Natural
     renames Ada.Strings.Unbounded.Length;

   function Substr
     (Str   : VString;
      Start : Positive;
      Len   : Natural) return  VString;
   function Substr
     (Str   : String;
      Start : Positive;
      Len   : Natural) return  VString;
   --  Returns the substring starting at the given character position (which
   --  is always counted from the start of the string, regardless of bounds,
   --  e.g. 2 means starting with the second character of the string), and
   --  with the length (Len) given. Indexing_Error is raised if the starting
   --  position is out of range, and Length_Error is raised if Len is too long.

   function Trim (Str : VString) return VString;
   function Trim (Str : String)  return VString;
   --  Returns the string obtained by removing all spaces from the right
   --  hand side of the string Str.

   procedure Trim (Str : in out VString);
   --  The procedure form is identical to the function form, except that the
   --  result overwrites the input argument Str.

   -----------------------
   -- Utility Functions --
   -----------------------

   --  In SPITBOL, integer values can be freely treated as strings. The
   --  following definitions help provide some of this capability in
   --  some common cases.

   function "&" (Num : Integer; Str : String)  return String;
   function "&" (Str : String;  Num : Integer) return String;
   function "&" (Num : Integer; Str : VString) return VString;
   function "&" (Str : VString; Num : Integer) return VString;
   --  In all these concatenation operations, the integer is converted to
   --  its corresponding decimal string form, with no leading blank.

   function S (Num : Integer) return String;
   function V (Num : Integer) return VString;
   --  These operators return the given integer converted to its decimal
   --  string form with no leading blank.

   function N (Str : VString) return Integer;
   --  Converts string to number (same as Integer'Value (S (Str)))

   -------------------
   -- Table Support --
   -------------------

   --  So far, we only provide support for tables whose indexing data values
   --  are strings (or unbounded strings). The values stored may be of any
   --  type, as supplied by the generic formal parameter.

   generic

      type Value_Type is private;
      --  Any non-limited type can be used as the value type in the table

      Null_Value : Value_Type;
      --  Value used to represent a value that is not present in the table

      with function Img (A : Value_Type) return String;
      --  Used to provide image of value in Dump procedure

      with function "=" (A, B : Value_Type) return Boolean is <>;
      --  This allows a user-defined equality function to override the
      --  predefined equality function.

   package Table is

      ------------------------
      -- Table Declarations --
      ------------------------

      type Table (N : Unsigned_32) is private;
      --  This is the table type itself. A table is a mapping from string
      --  values to values of Value_Type. The discriminant is an estimate of
      --  the number of values in the table. If the estimate is much too
      --  high, some space is wasted, if the estimate is too low, access to
      --  table elements is slowed down. The type Table has copy semantics,
      --  not reference semantics. This means that if a table is copied
      --  using simple assignment, then the two copies refer to entirely
      --  separate tables.

      -----------------------------
      -- Table Access Operations --
      -----------------------------

      function Get (T : Table; Name : VString)   return Value_Type;
      function Get (T : Table; Name : Character) return Value_Type;
      pragma Inline (Get);
      function Get (T : Table; Name : String)    return Value_Type;

      --  If an entry with the given name exists in the table, then the
      --  corresponding Value_Type value is returned. Otherwise Null_Value
      --  is returned.

      function Present (T : Table; Name : VString)   return Boolean;
      function Present (T : Table; Name : Character) return Boolean;
      pragma Inline (Present);
      function Present (T : Table; Name : String)    return Boolean;
      --  Determines if an entry with the given name is present in the table.
      --  A returned value of True means that it is in the table, otherwise
      --  False indicates that it is not in the table.

      procedure Delete (T : in out Table; Name : VString);
      procedure Delete (T : in out Table; Name : Character);
      pragma Inline (Delete);
      procedure Delete (T : in out Table; Name : String);
      --  Deletes the table element with the given name from the table. If
      --  no element in the table has this name, then the call has no effect.

      procedure Set (T : in out Table; Name  : VString;   Value : Value_Type);
      procedure Set (T : in out Table; Name  : Character; Value : Value_Type);
      pragma Inline (Set);
      procedure Set (T : in out Table; Name  : String;    Value : Value_Type);
      --  Sets the value of the element with the given name to the given
      --  value. If Value is equal to Null_Value, the effect is to remove
      --  the entry from the table. If no element with the given name is
      --  currently in the table, then a new element with the given value
      --  is created.

      ----------------------------
      -- Allocation and Copying --
      ----------------------------

      --  Table is a controlled type, so that all storage associated with
      --  tables is properly reclaimed when a Table value is abandoned.
      --  Tables have value semantics rather than reference semantics as
      --  in Spitbol, i.e. when you assign a copy you end up with two
      --  distinct copies of the table, as though COPY had been used in
      --  Spitbol. It seems clearly more appropriate in Ada to require
      --  the use of explicit pointers for reference semantics.

      procedure Clear (T : in out Table);
      --  Clears all the elements of the given table, freeing associated
      --  storage. On return T is an empty table with no elements.

      procedure Copy (From : Table; To : in out Table);
      --  First all the elements of table To are cleared (as described for
      --  the Clear procedure above), then all the elements of table From
      --  are copied into To. In the case where the tables From and To have
      --  the same declared size (i.e. the same discriminant), the call to
      --  Copy has the same effect as the assignment of From to To. The
      --  difference is that, unlike the assignment statement, which will
      --  cause a Constraint_Error if the source and target are of different
      --  sizes, Copy works fine with different sized tables.

      ----------------
      -- Conversion --
      ----------------

      type Table_Entry is record
         Name  : VString;
         Value : Value_Type;
      end record;

      type Table_Array is array (Positive range <>) of Table_Entry;

      function Convert_To_Array (T : Table) return Table_Array;
      --  Returns a Table_Array value with a low bound of 1, and a length
      --  corresponding to the number of elements in the table. The elements
      --  of the array give the elements of the table in unsorted order.

      ---------------
      -- Debugging --
      ---------------

      procedure Dump (T : Table; Str : String := "Table");
      --  Dump contents of given table to the standard output file. The
      --  string value Str is used as the name of the table in the dump.

      procedure Dump (T : Table_Array; Str : String := "Table_Array");
      --  Dump contents of given table array to the current output file. The
      --  string value Str is used as the name of the table array in the dump.

   private

      ------------------
      -- Private Part --
      ------------------

      --  A Table is a pointer to a hash table which contains the indicated
      --  number of hash elements (the number is forced to the next odd value
      --  if it is even to improve hashing performance). If more than one
      --  of the entries in a table hashes to the same slot, the Next field
      --  is used to chain entries from the header. The chains are not kept
      --  ordered. A chain is terminated by a null pointer in Next. An unused
      --  chain is marked by an element whose Name is null and whose value
      --  is Null_Value.

      type Hash_Element;
      type Hash_Element_Ptr is access all Hash_Element;

      type Hash_Element is record
         Name  : String_Access    := null;
         Value : Value_Type       := Null_Value;
         Next  : Hash_Element_Ptr := null;
      end record;

      type Hash_Table is
        array (Unsigned_32 range <>) of aliased Hash_Element;

      type Table (N : Unsigned_32) is new Controlled with record
         Elmts : Hash_Table (1 .. N);
      end record;

      pragma Finalize_Storage_Only (Table);

      procedure Adjust (Object : in out Table);
      --  The Adjust procedure does a deep copy of the table structure
      --  so that the effect of assignment is, like other assignments
      --  in Ada, value-oriented.

      procedure Finalize (Object : in out Table);
      --  This is the finalization routine that ensures that all storage
      --  associated with a table is properly released when a table object
      --  is abandoned and finalized.

   end Table;

end GNAT.Spitbol;

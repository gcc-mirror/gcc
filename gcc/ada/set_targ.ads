------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E T _ T A R G                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2013-2025, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package handles setting target dependent parameters. If the -gnatet
--  switch is not set, then these values are taken from the back end (via the
--  routines in Get_Targ, and the enumerate_modes routine in misc.cc). If the
--  switch is set, then the values are read from the target.atp file in the
--  current directory (usually written with the Write_Target_Dependent_Values
--  procedure defined in this package).

--  Note that all these values return sizes of C types with corresponding
--  names. This allows GNAT to define the corresponding Ada types to have
--  the same representation. There is one exception: the representation
--  of Wide_Character_Type uses twice the size of a C char, instead of the
--  size of wchar_t, since this corresponds to expected Ada usage.

with Stand; use Stand;
with Types; use Types;

package Set_Targ is

   -----------------------------
   -- Target-Dependent Values --
   -----------------------------

   --  The following is a table of target dependent values. In normal operation
   --  these values are set by calling the appropriate C backend routines that
   --  interface to back end routines that determine target characteristics.

   --  If the -gnateT switch is used, then any values that are read from the
   --  file target.atp in the current directory overwrite values set from the
   --  back end. This is used by tools other than the compiler, e.g. to do
   --  semantic analysis of programs that will run on some other target than
   --  the machine on which the tool is run.

   --  Note: fields marked with a question mark are boolean fields, where a
   --  value of 0 is False, and a value of 1 is True.

   Bits_BE                    : Nat; -- Bits stored big-endian?
   Bits_Per_Unit              : Pos; -- Bits in a storage unit
   Bits_Per_Word              : Pos; -- Bits in a word
   Bytes_BE                   : Nat; -- Bytes stored big-endian?
   Char_Size                  : Pos; -- Standard.Character'Size
   Double_Float_Alignment     : Nat; -- Alignment of double float
   Double_Scalar_Alignment    : Nat; -- Alignment of double length scalar
   Double_Size                : Pos; -- Standard.Long_Float'Size
   Float_Size                 : Pos; -- Standard.Float'Size
   Float_Words_BE             : Nat; -- Float words stored big-endian?
   Int_Size                   : Pos; -- Standard.Integer'Size
   Long_Double_Size           : Pos; -- Standard.Long_Long_Float'Size
   Long_Long_Long_Size        : Pos; -- Standard.Long_Long_Long_Integer'Size
   Long_Long_Size             : Pos; -- Standard.Long_Long_Integer'Size
   Long_Size                  : Pos; -- Standard.Long_Integer'Size
   Maximum_Alignment          : Pos; -- Maximum permitted alignment
   Max_Unaligned_Field        : Pos; -- Kept only for backward compatibility
   Pointer_Size               : Pos; -- System.Address'Size
   Short_Enums                : Nat; -- Foreign enums use short size?
   Short_Size                 : Pos; -- Standard.Short_Integer'Size
   Strict_Alignment           : Nat; -- Strict alignment?
   System_Allocator_Alignment : Nat; -- Alignment for malloc calls
   Wchar_T_Size               : Pos; -- Interfaces.C.wchar_t'Size
   Words_BE                   : Nat; -- Words stored big-endian?

   -------------------------------------
   -- Registered Floating-Point Types --
   -------------------------------------

   --  This table contains the list of modes supported by the back-end as
   --  provided by the back end routine enumerate_modes in misc.cc. Note that
   --  we only store floating-point modes (see Register_Float_Type).

   type FPT_Mode_Entry is record
      NAME      : String_Ptr;     -- Name of mode (no null character at end)
      DIGS      : Natural;        -- Digits for floating-point type
      FLOAT_REP : Float_Rep_Kind; -- Float representation
      PRECISION : Natural;        -- Precision in bits
      SIZE      : Natural;        -- Size in bits
      ALIGNMENT : Natural;        -- Alignment in bits
   end record;

   FPT_Mode_Table : array (1 .. 1000) of FPT_Mode_Entry;
   Num_FPT_Modes  : Natural := 0;
   --  Table containing the supported modes and number of entries

   -----------------
   -- Subprograms --
   -----------------

   subtype S_Float_Types is
     Standard_Entity_Type range S_Short_Float .. S_Long_Long_Float;

   function C_Type_For (T : S_Float_Types) return String;
   --  Return the name of a C type supported by the back-end and suitable as
   --  a basis to construct the standard Ada floating point type identified by
   --  T. This is used as a common ground to feed both ttypes values and the
   --  GNAT tree nodes for the standard floating point types.

   procedure Write_Target_Dependent_Values;
   --  This routine writes the file target.atp in the current directory with
   --  the values of the global target parameters as listed above, and as set
   --  by prior calls to Initialize/Read_Target_Dependent_Values. The format
   --  of the target.atp file is as follows
   --
   --    First come the values of the variables defined in this spec:
   --
   --      One line per value
   --
   --        name  value
   --
   --      where name is the name of the parameter, spelled out in full,
   --      and cased as in the above list, and value is an unsigned decimal
   --      integer. Two or more blanks separates the name from the value.
   --
   --      All the variables must be present, in alphabetical order (i.e. the
   --      same order as the declarations in this spec).
   --
   --   Then there is a blank line to separate the two parts of the file. Then
   --   come the lines showing the floating-point types to be registered.
   --
   --     One line per registered mode
   --
   --       name  digs float_rep precision alignment
   --
   --     where name is the string name of the type (which can have
   --     single spaces embedded in the name (e.g. long double). The
   --     name is followed by at least two blanks. The following fields
   --     are as described above for a Mode_Entry (where float_rep is
   --     I for IEEE-754-Binary, which is the only Float_Rep_Kind
   --     currently supported), fields are separated by at least one
   --     blank, and a LF character immediately follows the alignment
   --     field.
   --
   --     ??? We do not write the size for backward compatibility reasons,
   --     which means that target.atp will not be a complete description for
   --     the very peculiar cases where the size cannot be computed from the
   --     precision and the alignment by the formula:
   --
   --       size := (precision + alignment - 1) / alignment * alignment

end Set_Targ;

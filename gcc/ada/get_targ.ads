------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             G E T _ T A R G                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2021, Free Software Foundation, Inc.         --
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

--  This package provides an Import to the C functions which provide
--  values related to types on the target system. It is only needed for
--  exp_dbug and the elaboration of ttypes, via the Set_Targs package.
--  It also contains the routine for registering floating-point types.

--  NOTE: Any changes in this package must be reflected in aa_getta.adb
--  and any other version in the various back ends.

--  Note that all these values return sizes of C types with corresponding
--  names. This allows GNAT to define the corresponding Ada types to have
--  the same representation. There is one exception to this general rule:
--  the Wide_Character_Type uses twice the size of a C char, instead of the
--  size of wchar_t.

with Types; use Types;

package Get_Targ is

   --  Functions returning individual runtime values

   function Get_Bits_Per_Unit              return Pos;
   --  System.Storage_Unit

   function Get_Bits_Per_Word              return Pos;
   --  System.Word_Size

   function Get_Char_Size                  return Pos;
   --  Size of Standard.Character

   function Get_Wchar_T_Size               return Pos;
   --  Size of Interfaces.C.wchar_t

   function Get_Short_Size                 return Pos;
   --  Size of Standard.Short_Integer

   function Get_Int_Size                   return Pos;
   --  Size of Standard.Integer

   function Get_Long_Size                  return Pos;
   --  Size of Standard.Long_Integer

   function Get_Long_Long_Size             return Pos;
   --  Size of Standard.Long_Long_Integer

   function Get_Long_Long_Long_Size        return Pos;
   --  Size of Standard.Long_Long_Long_Integer

   function Get_Pointer_Size               return Pos;
   --  Size of System.Address

   function Get_Maximum_Alignment          return Pos;
   --  Maximum supported alignment

   function Get_Float_Words_BE             return Nat;
   --  Non-zero iff float words big endian

   function Get_Words_BE                   return Nat;
   --  Non-zero iff integer words big endian

   function Get_Bytes_BE                   return Nat;
   --  Non-zero iff bytes big-endian

   function Get_Bits_BE                    return Nat;
   --  Non-zero iff bit order big endian

   function Get_Strict_Alignment           return Nat;
   --  Non-zero if target requires strict alignent

   function Get_System_Allocator_Alignment return Nat;
   --  Alignment guaranteed by malloc falls

   function Get_Double_Float_Alignment     return Nat;
   --  Alignment required for Long_Float or 0 if no special requirement

   function Get_Double_Scalar_Alignment    return Nat;
   --  Alignment required for Long_Long_Integer or larger integer types
   --  or 0 if no special requirement.

   function Get_Short_Enums                return Int;
   --  Returns non-zero if we are in short enums mode, where foreign convention
   --  (in particular C and C++) enumeration types will be sized as in Ada,
   --  using the shortest possibility from 8,16,32 bits, signed or unsigned.
   --  A zero value means Short_Enums are not in use, and in this case all
   --  foreign convention enumeration types are given the same size as c int.

   --  Other subprograms

   function Get_Max_Unaligned_Field return Pos;
   --  Returns the maximum supported size in bits for a field that is
   --  not aligned on a storage unit boundary.

   type C_String is array (0 .. 255) of aliased Character;
   pragma Convention (C, C_String);

   type Register_Type_Proc is access procedure
     (C_Name    : C_String;       -- Nul-terminated string with name of type
      Digs      : Natural;        -- Digits for floating point, 0 otherwise
      Complex   : Boolean;        -- True iff type has real and imaginary parts
      Count     : Natural;        -- Number of elements in vector, 0 otherwise
      Float_Rep : Float_Rep_Kind; -- Representation used for fpt type
      Precision : Positive;       -- Precision of representation in bits
      Size      : Positive;       -- Size of representation in bits
      Alignment : Natural);       -- Required alignment in bits
   pragma Convention (C, Register_Type_Proc);
   --  Call back procedure for Register_Back_End_Types. This is to be used by
   --  Create_Standard to create predefined types for all types supported by
   --  the back end.

   procedure Register_Back_End_Types (Call_Back : Register_Type_Proc);
   --  Calls the Call_Back function with information for each supported type

   function Get_Back_End_Config_File return String_Ptr;
   --  Return the back end configuration file, or null if none. If non-null,
   --  this file should be used instead of calling the various Get_xxx
   --  functions in this package.

end Get_Targ;

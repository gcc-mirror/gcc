------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--             S Y S T E M . S T R E A M _ A T T R I B U T E S              --
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

--  This package contains the implementations of the stream attributes for
--  elementary types. These are the subprograms that are directly accessed
--  by occurrences of the stream attributes where the type is elementary.

--  We only provide the subprograms for the standard base types. For user
--  defined types, the subprogram for the corresponding root type is called
--  with an appropriate conversion.

with System;
with System.Unsigned_Types;
with Ada.Streams;

package System.Stream_Attributes is
pragma Preelaborate (Stream_Attributes);

   pragma Suppress (Accessibility_Check, Stream_Attributes);
   --  No need to check accessibility on arguments of subprograms

   package UST renames System.Unsigned_Types;

   subtype RST is Ada.Streams.Root_Stream_Type'Class;

   subtype SEC is Ada.Streams.Stream_Element_Count;

   --  Enumeration types are usually transferred using the routine for the
   --  corresponding integer. The exception is that special routines are
   --  provided for Boolean and the character types, in case the protocol
   --  in use provides specially for these types.

   --  Access types use either a thin pointer (single address) or fat pointer
   --  (double address) form. The following types are used to hold access
   --  values using unchecked conversions.

   type Thin_Pointer is record
      P1 : System.Address;
   end record;

   type Fat_Pointer is record
      P1 : System.Address;
      P2 : System.Address;
   end record;

   ------------------------------------
   -- Treatment of enumeration types --
   ------------------------------------

   --  In this interface, there are no specific routines for general input
   --  or output of enumeration types. Generally, enumeration types whose
   --  representation is unsigned (no negative representation values) are
   --  treated as unsigned integers, and enumeration types that do have
   --  negative representation values are treated as signed integers.

   --  An exception is that there are specialized routines for Boolean,
   --  Character, and Wide_Character types, but these specialized routines
   --  are used only if the type in question has a standard representation.
   --  For the case of a non-standard representation (one where the size of
   --  the first subtype is specified, or where an enumeration representation
   --  clause is given, these three types are treated like any other cases
   --  of enumeration types, as described above.
   --  for

   ---------------------
   -- Input Functions --
   ---------------------

   --  Functions for S'Input attribute. These functions are also used for
   --  S'Read, with the obvious transformation, since the input operation
   --  is the same for all elementary types (no bounds or discriminants
   --  are involved).

   function I_AD  (Stream : access RST) return Fat_Pointer;
   function I_AS  (Stream : access RST) return Thin_Pointer;
   function I_B   (Stream : access RST) return Boolean;
   function I_C   (Stream : access RST) return Character;
   function I_F   (Stream : access RST) return Float;
   function I_I   (Stream : access RST) return Integer;
   function I_LF  (Stream : access RST) return Long_Float;
   function I_LI  (Stream : access RST) return Long_Integer;
   function I_LLF (Stream : access RST) return Long_Long_Float;
   function I_LLI (Stream : access RST) return Long_Long_Integer;
   function I_LLU (Stream : access RST) return UST.Long_Long_Unsigned;
   function I_LU  (Stream : access RST) return UST.Long_Unsigned;
   function I_SF  (Stream : access RST) return Short_Float;
   function I_SI  (Stream : access RST) return Short_Integer;
   function I_SSI (Stream : access RST) return Short_Short_Integer;
   function I_SSU (Stream : access RST) return UST.Short_Short_Unsigned;
   function I_SU  (Stream : access RST) return UST.Short_Unsigned;
   function I_U   (Stream : access RST) return UST.Unsigned;
   function I_WC  (Stream : access RST) return Wide_Character;

   -----------------------
   -- Output Procedures --
   -----------------------

   --  Procedures for S'Write attribute. These procedures are also used
   --  for 'Output, since for elementary types there is no difference
   --  between 'Write and 'Output because there are no discriminants
   --  or bounds to be written.

   procedure W_AD  (Stream : access RST; Item : in Fat_Pointer);
   procedure W_AS  (Stream : access RST; Item : in Thin_Pointer);
   procedure W_B   (Stream : access RST; Item : in Boolean);
   procedure W_C   (Stream : access RST; Item : in Character);
   procedure W_F   (Stream : access RST; Item : in Float);
   procedure W_I   (Stream : access RST; Item : in Integer);
   procedure W_LF  (Stream : access RST; Item : in Long_Float);
   procedure W_LI  (Stream : access RST; Item : in Long_Integer);
   procedure W_LLF (Stream : access RST; Item : in Long_Long_Float);
   procedure W_LLI (Stream : access RST; Item : in Long_Long_Integer);
   procedure W_LLU (Stream : access RST; Item : in UST.Long_Long_Unsigned);
   procedure W_LU  (Stream : access RST; Item : in UST.Long_Unsigned);
   procedure W_SF  (Stream : access RST; Item : in Short_Float);
   procedure W_SI  (Stream : access RST; Item : in Short_Integer);
   procedure W_SSI (Stream : access RST; Item : in Short_Short_Integer);
   procedure W_SSU (Stream : access RST; Item : in UST.Short_Short_Unsigned);
   procedure W_SU  (Stream : access RST; Item : in UST.Short_Unsigned);
   procedure W_U   (Stream : access RST; Item : in UST.Unsigned);
   procedure W_WC  (Stream : access RST; Item : in Wide_Character);

   ----------------------------
   -- Composite Input/Output --
   ----------------------------

   --  The following Boolean constant is defined and set to True only if the
   --  stream representation of a series of elementary items of the same
   --  type (one of the types handled by the above procedures) has the same
   --  representation as an array of such items in memory. This allows such
   --  a series of items to be read or written as a block, instead of
   --  element by element.

   --  If the stream representation does not have this property for all the
   --  above types, then this constant can be omitted or set to False,
   --  and the front end will generate element-by-element operations.

   --  This interface assumes that a Stream_Element has the same size as
   --  a Storage_Unit. If that is not the case, then this flag should
   --  also be omitted (or set to False).

   Block_Stream_Ops_OK : constant Boolean := True;
   --  Set to False if block stream operations not permitted

private
   pragma Inline (I_AD);
   pragma Inline (I_AS);
   pragma Inline (I_B);
   pragma Inline (I_C);
   pragma Inline (I_F);
   pragma Inline (I_I);
   pragma Inline (I_LF);
   pragma Inline (I_LI);
   pragma Inline (I_LLF);
   pragma Inline (I_LLI);
   pragma Inline (I_LLU);
   pragma Inline (I_LU);
   pragma Inline (I_SF);
   pragma Inline (I_SI);
   pragma Inline (I_SSI);
   pragma Inline (I_SSU);
   pragma Inline (I_SU);
   pragma Inline (I_U);
   pragma Inline (I_WC);

   pragma Inline (W_AD);
   pragma Inline (W_AS);
   pragma Inline (W_B);
   pragma Inline (W_C);
   pragma Inline (W_F);
   pragma Inline (W_I);
   pragma Inline (W_LF);
   pragma Inline (W_LI);
   pragma Inline (W_LLF);
   pragma Inline (W_LLI);
   pragma Inline (W_LLU);
   pragma Inline (W_LU);
   pragma Inline (W_SF);
   pragma Inline (W_SI);
   pragma Inline (W_SSI);
   pragma Inline (W_SSU);
   pragma Inline (W_SU);
   pragma Inline (W_U);
   pragma Inline (W_WC);

end System.Stream_Attributes;

------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--             S Y S T E M . S T R E A M _ A T T R I B U T E S              --
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
   pragma Preelaborate;

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
   --  clause is given), these three types are treated like any other cases
   --  of enumeration types, as described above.

   ---------------------
   -- Input Functions --
   ---------------------

   --  Functions for S'Input attribute. These functions are also used for
   --  S'Read, with the obvious transformation, since the input operation
   --  is the same for all elementary types (no bounds or discriminants
   --  are involved).

   function I_AD  (Stream : not null access RST) return Fat_Pointer;
   function I_AS  (Stream : not null access RST) return Thin_Pointer;
   function I_B   (Stream : not null access RST) return Boolean;
   function I_C   (Stream : not null access RST) return Character;
   function I_F   (Stream : not null access RST) return Float;
   function I_I   (Stream : not null access RST) return Integer;
   function I_LF  (Stream : not null access RST) return Long_Float;
   function I_LI  (Stream : not null access RST) return Long_Integer;
   function I_LLF (Stream : not null access RST) return Long_Long_Float;
   function I_LLI (Stream : not null access RST) return Long_Long_Integer;
   function I_LLU (Stream : not null access RST) return UST.Long_Long_Unsigned;
   function I_LU  (Stream : not null access RST) return UST.Long_Unsigned;
   function I_SF  (Stream : not null access RST) return Short_Float;
   function I_SI  (Stream : not null access RST) return Short_Integer;
   function I_SSI (Stream : not null access RST) return Short_Short_Integer;
   function I_SSU (Stream : not null access RST) return
                                                   UST.Short_Short_Unsigned;
   function I_SU  (Stream : not null access RST) return UST.Short_Unsigned;
   function I_U   (Stream : not null access RST) return UST.Unsigned;
   function I_WC  (Stream : not null access RST) return Wide_Character;
   function I_WWC (Stream : not null access RST) return Wide_Wide_Character;

   -----------------------
   -- Output Procedures --
   -----------------------

   --  Procedures for S'Write attribute. These procedures are also used for
   --  'Output, since for elementary types there is no difference between
   --  'Write and 'Output because there are no discriminants or bounds to
   --  be written.

   procedure W_AD  (Stream : not null access RST; Item : Fat_Pointer);
   procedure W_AS  (Stream : not null access RST; Item : Thin_Pointer);
   procedure W_B   (Stream : not null access RST; Item : Boolean);
   procedure W_C   (Stream : not null access RST; Item : Character);
   procedure W_F   (Stream : not null access RST; Item : Float);
   procedure W_I   (Stream : not null access RST; Item : Integer);
   procedure W_LF  (Stream : not null access RST; Item : Long_Float);
   procedure W_LI  (Stream : not null access RST; Item : Long_Integer);
   procedure W_LLF (Stream : not null access RST; Item : Long_Long_Float);
   procedure W_LLI (Stream : not null access RST; Item : Long_Long_Integer);
   procedure W_LLU (Stream : not null access RST; Item :
                                                    UST.Long_Long_Unsigned);
   procedure W_LU  (Stream : not null access RST; Item : UST.Long_Unsigned);
   procedure W_SF  (Stream : not null access RST; Item : Short_Float);
   procedure W_SI  (Stream : not null access RST; Item : Short_Integer);
   procedure W_SSI (Stream : not null access RST; Item : Short_Short_Integer);
   procedure W_SSU (Stream : not null access RST; Item :
                                                    UST.Short_Short_Unsigned);
   procedure W_SU  (Stream : not null access RST; Item : UST.Short_Unsigned);
   procedure W_U   (Stream : not null access RST; Item : UST.Unsigned);
   procedure W_WC  (Stream : not null access RST; Item : Wide_Character);
   procedure W_WWC (Stream : not null access RST; Item : Wide_Wide_Character);

   function Block_IO_OK return Boolean;
   --  Package System.Stream_Attributes has several bodies - the default one
   --  distributed with GNAT, and s-stratt-xdr.adb, which is based on the XDR
   --  standard. Both bodies share the same spec. The role of this function is
   --  to indicate whether the current version of System.Stream_Attributes
   --  supports block IO. See System.Strings.Stream_Ops (s-ststop) for details.

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
   pragma Inline (I_WWC);

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
   pragma Inline (W_WWC);

   pragma Inline (Block_IO_OK);

end System.Stream_Attributes;

------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--          S Y S T E M . S T R E A M _ A T T R I B U T E S . X D R         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2025, Free Software Foundation, Inc.         --
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

--  This package contains alternate implementations of the stream attributes
--  for elementary types based on the XDR standard. These are the subprograms
--  that are directly accessed by occurrences of the stream attributes where
--  the type is elementary.

--  It is especially useful for exchanging streams between two different
--  systems with different basic type representations and endianness.

--  We only provide the subprograms for the standard base types. For user
--  defined types, the subprogram for the corresponding root type is called
--  with an appropriate conversion.

package System.Stream_Attributes.XDR is
   pragma Preelaborate;

   pragma Suppress (Accessibility_Check, XDR);
   --  No need to check accessibility on arguments of subprograms

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
   function I_I24 (Stream : not null access RST) return Integer_24;
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
   function I_U24 (Stream : not null access RST) return Unsigned_24;
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
   procedure W_I24 (Stream : not null access RST; Item : Integer_24);
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
   procedure W_U24 (Stream : not null access RST; Item : Unsigned_24);
   procedure W_WC  (Stream : not null access RST; Item : Wide_Character);
   procedure W_WWC (Stream : not null access RST; Item : Wide_Wide_Character);

end System.Stream_Attributes.XDR;

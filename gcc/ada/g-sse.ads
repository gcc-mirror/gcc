------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             G N A T . S S E                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 2009, Free Software Foundation, Inc.           --
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

--  This package is the root of a set aimed at offering Ada bindings to a
--  subset of the Intel(r) Streaming SIMD Extensions with GNAT. The purpose is
--  to allow access from Ada to the SSE facilities defined in the Intel(r)
--  compiler manuals, in particular in the Intrinsics Reference of the C++
--  Compiler User's Guide, available from http://www.intel.com.

--  Assuming actual hardware support is available, this capability is
--  currently supported on the following set of targets:

--     GNU/Linux x86 and x86_64
--     Windows XP/Vista x86 and x86_64

--  This unit exposes vector _component_ types together with general comments
--  on the binding contents.

--  As of today, one other unit is offered: GNAT.SSE.Vector__Types, which
--  exposes Ada types corresponding to the reference types (__m128 and the
--  like) over which GCC builtins will operate. The exposed Ada types are
--  private. Object initializations or value observations may be performed
--  with unchecked conversions or address overlays, for example:

--  with Ada.Unchecked_Conversion;
--  with GNAT.SSE.Vector_Types; use GNAT.SSE; use GNAT.SSE.Vector_Types;

--  procedure SSE_Base is

--     --  Core operations

--     function mm_add_ss (A, B : M128) return M128;
--     pragma Import (Intrinsic, mm_add_ss, "__builtin_ia32_addss");

--     --  User views / conversions or overlays

--     type Vf32_View is array (1 .. 4) of Float;
--     for Vf32_View'Alignment use VECTOR_ALIGN;

--     function To_M128 is new Ada.Unchecked_Conversion (Vf32_View, M128);

--     X, Y, Z : M128;

--     Vz : Vf32_View;
--     for Vz'Address use Z'Address;

--  begin
--     X := To_M128 ((1.0, 1.0, 2.0, 2.0));
--     Y := To_M128 ((2.0, 2.0, 1.0, 1.0));
--     Z := mm_add_ss (X, Y);

--     if vz /= (3.0, 1.0, 2.0, 2.0) then
--        raise Program_Error;
--     end if;
--  end;

package GNAT.SSE is
   type Float32 is new Float;
   type Float64 is new Long_Float;
   type Integer64 is new Long_Long_Integer;

   VECTOR_BYTES : constant := 16;
   --  Common size of all the SSE vector types, in bytes.

   VECTOR_ALIGN : constant := 16;
   --  Common alignment of all the SSE vector types, in bytes.

   --  Alignment-wise, the reference document reads:
   --  << The compiler aligns __m128d and _m128i local and global data to
   --     16-byte boundaries on the stack. >>
   --
   --  We apply that consistently to all the Ada vector types, as GCC does
   --  for the corresponding C types.

end GNAT.SSE;

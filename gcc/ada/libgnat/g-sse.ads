------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             G N A T . S S E                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2009-2023, Free Software Foundation, Inc.         --
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
--  subset of the Intel(r) Streaming SIMD Extensions with GNAT. The purpose
--  is to allow access from Ada to the SSE facilities defined in the Intel(r)
--  compiler manuals, in particular in the Intrinsics Reference of the C++
--  Compiler User's Guide, available from http://www.intel.com.

--  Assuming actual hardware support is available, this capability is
--  currently supported on the following set of targets:

--     GNU/Linux x86 and x86_64
--     Windows XP/Vista x86 and x86_64
--     Solaris x86
--     Darwin x86_64

--  This unit exposes vector _component_ types together with general comments
--  on the binding contents.

--  One other unit is offered as of today: GNAT.SSE.Vector_Types, which
--  exposes Ada types corresponding to the reference types (__m128 and the
--  like) over which a binding to the SSE GCC builtins may operate.

--  The exposed Ada types are private. Object initializations or value
--  observations may be performed with unchecked conversions or address
--  overlays, for example:

--  with Ada.Unchecked_Conversion;
--  with GNAT.SSE.Vector_Types; use GNAT.SSE, GNAT.SSE.Vector_Types;

--  procedure SSE_Base is

--     --  Core operations

--     function ia32_addps (A, B : m128) return m128;
--     pragma Import (Intrinsic, ia32_addps, "__builtin_ia32_addps");

--     --  User views & conversions

--     type Vf32_View is array (1 .. 4) of GNAT.SSE.Float32;
--     for Vf32_View'Alignment use VECTOR_ALIGN;

--     function To_m128 is new Ada.Unchecked_Conversion (Vf32_View, m128);

--     Xf32 : constant Vf32_View := (1.0, 1.0, 2.0, 2.0);
--     Yf32 : constant Vf32_View := (2.0, 2.0, 1.0, 1.0);

--     X128 : constant m128 := To_m128 (Xf32);
--     Y128 : constant m128 := To_m128 (Yf32);

--  begin
--     --  Operations & overlays

--     declare
--        Z128 : m128;
--        Zf32 : Vf32_View;
--        for Zf32'Address use Z128'Address;
--     begin
--        Z128 := ia32_addps (X128, Y128);
--        if Zf32 /= (3.0, 3.0, 3.0, 3.0) then
--           raise Program_Error;
--        end if;
--     end;

--     declare
--        type m128_View_Kind is (SSE, F32);
--        type m128_Object (View : m128_View_Kind := F32) is record
--           case View is
--              when SSE  => V128 : m128;
--              when F32  => Vf32 : Vf32_View;
--           end case;
--        end record;
--        pragma Unchecked_Union (m128_Object);

--        O1 : constant m128_Object := (View => SSE, V128 => X128);
--     begin
--        if O1.Vf32 /= Xf32 then
--           raise Program_Error;
--        end if;
--     end;
--  end SSE_Base;

package GNAT.SSE is

   -----------------------------------
   -- Common vector characteristics --
   -----------------------------------

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

   ----------------------------
   -- Vector component types --
   ----------------------------

   type Float32 is new Float;
   type Float64 is new Long_Float;
   type Integer64 is new Long_Long_Integer;

end GNAT.SSE;

--  { dg-do run { target i?86-*-* x86_64-*-* } }
--  { dg-options "-O1 -msse" }
--  { dg-require-effective-target sse } 

with Ada.Unchecked_Conversion;

procedure SSE_Nolib is

   --  Base vector type definitions

   package SSE_Types is
      VECTOR_ALIGN : constant := 16;
      VECTOR_BYTES : constant := 16;
            
      type m128 is private;
   private
      type m128 is array (1 .. 4) of Float;
      for m128'Alignment use VECTOR_ALIGN;
      pragma Machine_Attribute (m128, "vector_type");
      pragma Machine_Attribute (m128, "may_alias");
   end SSE_Types;

   use SSE_Types;

   --  Core operations

   function mm_add_ss (A, B : m128) return m128;
   pragma Import (Intrinsic, mm_add_ss, "__builtin_ia32_addss");

   --  User views / conversions or overlays

   type Vf32_View is array (1 .. 4) of Float;
   for Vf32_View'Alignment use VECTOR_ALIGN;

   function To_m128 is new Ada.Unchecked_Conversion (Vf32_View, m128);
   function To_m128 is new Ada.Unchecked_Conversion (m128, Vf32_View);

   X, Y, Z : M128;

   Vz : Vf32_View;
   for Vz'Address use Z'Address;
begin
   X := To_m128 ((1.0, 1.0, 2.0, 2.0));
   Y := To_m128 ((2.0, 2.0, 1.0, 1.0));
   Z := mm_add_ss (X, Y);

   if Vz /= (3.0, 1.0, 2.0, 2.0) then
      raise Program_Error;
   end if;
end SSE_Nolib;

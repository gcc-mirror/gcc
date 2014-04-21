-- { dg-do compile { target i?86-*-* x86_64-*-* } }
-- { dg-options "-fdump-tree-optimized" }

with Interfaces;
with Unchecked_Conversion;

with GNAT.SSE.Vector_Types; use GNAT.SSE.Vector_Types;

procedure Vect14 is

  Msk1  : constant := 16#000FFAFFFFFFFB3F#;
  Msk2  : constant := 16#000FFDFFFC90FFFD#;

  type Unsigned_64_Array_Type is array (1 .. 2) of Interfaces.Unsigned_64;

  function Convert is new Unchecked_Conversion (Unsigned_64_Array_Type, M128i);

  Sse2_Param_Mask : constant M128i := Convert ((Msk1, Msk2));

begin
  null;
end;

-- { dg-final { scan-tree-dump-not "VIEW_CONVERT_EXPR" "optimized" } }
-- { dg-final { cleanup-tree-dump "optimized" } }

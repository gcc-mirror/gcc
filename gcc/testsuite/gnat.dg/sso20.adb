--  { dg-do run }
--  { dg-options "-O" }

with Ada.Unchecked_Conversion;
with Interfaces;  use Interfaces;
with System;      use System;

procedure SSO20 is

  type Bytes_Ref is array (1 .. 4) of Unsigned_8
    with Convention => Ada_Pass_By_Reference;

  type U32_BE is record
    Value : Unsigned_32;
  end record
    with
      Pack,
      Bit_Order            => High_Order_First,
      Scalar_Storage_Order => High_Order_First;

  function Conv is new Ada.Unchecked_Conversion (Bytes_Ref, U32_BE);

  function Value (B : Bytes_Ref) return Unsigned_32 is (Conv (B).Value);

begin
  if Value ((16#11#, 16#22#, 16#33#, 16#44#)) /= 16#11223344# then
     raise Program_Error;
  end if;
end;

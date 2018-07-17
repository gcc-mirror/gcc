-- { dg-do compile }

with System;
with Unchecked_Conversion;

procedure Warn5 is

  type Digit_Type is range 0..15;

  type Frequency_Type is array( 1..12) of Digit_Type;
  pragma Pack(Frequency_Type);

  type Element_Type is record
     F : Frequency_Type;
  end record;

  type Array_Type is array (Natural range <>) of Element_Type;

  type List_Type is record
    A : Array_Type (0..1);
  end record;
  for List_Type'Alignment use 4;

  type Pointer_Type is access Element_Type;
  function To_Ptr is new Unchecked_Conversion(System.Address, Pointer_Type);

  function Pointer (Pos : Natural; List : List_Type) return Pointer_Type is
  begin
    return To_Ptr(List.A(Pos)'Address); -- { dg-warning "source alignment" "" { target alpha*-*-* arm*-*-* hppa*-*-* ia64-*-* mips*-*-* riscv*-*-* sparc*-*-* } }
  end;

begin
  null;
end;

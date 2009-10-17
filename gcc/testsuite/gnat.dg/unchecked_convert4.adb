-- { dg-do compile }

with Unchecked_Conversion;

procedure Unchecked_Convert4 is

  type Uint32 is mod 2**32;

  type Rec is record
    I : Uint32;
  end record;
  for Rec'Size use 32;
  pragma Atomic (Rec);

  function Conv is new Unchecked_Conversion (Uint32, Rec);

  function F return Uint32;
  pragma Import (Ada, F);

  procedure Proc (R : Rec) is begin null; end;

begin
  Proc (Conv (F or 1));
end;

--  { dg-do run }
--  { dg-options "-gnatws -O" }

with System;

procedure SSO17 is

  type My_Float is new Float range 0.0 .. 359.99;

  type Rec is record
    Az : My_Float;
    El : My_Float;
  end record;
  for Rec'Bit_Order use System.High_Order_First;
  for Rec'Scalar_Storage_Order use System.High_Order_First;

  R : Rec;

  procedure Is_True (B : Boolean);
  pragma No_Inline (Is_True);

  procedure Is_True (B : Boolean) is
  begin
    if not B then
      raise Program_Error;
    end if;
  end;

begin
  R := (Az => 1.1, El => 2.2);
  Is_True (R.Az'Valid);
  R := (Az => 3.3, El => 4.4);
  Is_True (R.Az'Valid);
end;

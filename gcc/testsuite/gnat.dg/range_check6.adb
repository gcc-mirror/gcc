--  { dg-do run }
--  { dg-options "-O0 -gnatVa" }

procedure Range_Check6 is

  type Byte is range -2**7 .. 2**7-1;
  for Byte'Size use 8;

  subtype Hour is Byte range 0 .. 23;

  type Rec is record
    B : Byte;
  end record;

  procedure Encode (H : in out Hour) is
  begin
    null;
  end;

  R : Rec;

begin
  R.B := 24;
  Encode (R.B);
  raise Program_Error;
exception
  when Constraint_Error => null;
end;

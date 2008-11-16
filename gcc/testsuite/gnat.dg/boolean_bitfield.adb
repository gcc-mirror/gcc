-- { dg-do run }
-- { dg-options "-O" }

with System; use System;

procedure Boolean_Bitfield is

  Units_Per_Integer : constant :=
    (Integer'Size + System.Storage_Unit - 1) / System.Storage_Unit;

  type E_type is (Red, Blue, Green);

  type Parent_Type is record
    I : Integer range 0 .. 127 := 127;
    C : Character := 'S';
    B : Boolean := False;
    E : E_Type := Blue;
  end record;

  for Parent_Type use record
    C at 0 * Units_Per_Integer range 0 .. Character'Size - 1;
    B at 1 * Units_Per_Integer range 0 .. Boolean'Size - 1;
    I at 2 * Units_Per_Integer range 0 .. Integer'Size/2 - 1;
    E at 3 * Units_Per_Integer range 0 .. Character'Size - 1;
  end record;

  type Derived_Type is new Parent_Type;

  for Derived_Type use record
    C at 1 * Units_Per_Integer range 1 .. Character'Size + 1;
    B at 3 * Units_Per_Integer range 1 .. Boolean'Size + 1;
    I at 5 * Units_Per_Integer range 1 .. Integer'Size/2 + 1;
    E at 7 * Units_Per_Integer range 1 .. Character'Size + 1;
  end record;

  Rec   : Derived_Type;

begin
  Rec := (12, 'T', True, Red);

  if (Rec.I /= 12) or (Rec.C /= 'T') or (not Rec.B) or (Rec.E /= Red) then
    raise Program_Error;
  end if;
end;

--  { dg-do run }

with Ada.Characters.Latin_1;

procedure Component_Value1 is

  type Y_Array is array (Natural range <>) of Character
    with Default_Component_Value => Ada.Characters.Latin_1.Space;

  type Y2_Array is new Y_Array
    with Default_Component_Value => Ada.Characters.Latin_1.HT;

  type X_String is new String
    with Default_Component_Value => Ada.Characters.Latin_1.Space;

  Y  : Y_Array  (1..20);
  Y2 : Y2_Array (1..20);
  X  : X_String (1..20);

begin
  if not (for all I in Y'Range => Y(I) = Ada.Characters.Latin_1.Space) then
    raise Program_Error;
  end if;

  if not (for all I in Y2'Range => Y2(I) = Ada.Characters.Latin_1.HT) then
    raise Program_Error;
  end if;

  if not (for all I in X'Range => X(I) = Ada.Characters.Latin_1.Space) then
    raise Program_Error;
  end if;
end;

-- { dg-do run }

with Unchecked_Conversion;

procedure Unchecked_Convert10 is

  subtype Unsigned_Type is Integer range 2_034 .. 2_164;

  subtype Signed_Type is Integer range -2048 .. 2047; 

  function To_Signed_Type is
     new Unchecked_Conversion (Source => Unsigned_Type, Target => Signed_Type);

  function To_Unsigned_Type is
     new Unchecked_Conversion (Source => Signed_Type, Target => Unsigned_Type);

  Data : Unsigned_Type;
  Temp : Signed_Type;

begin

  Data := 2100;
  Temp := To_Signed_Type (Data);
  if Temp /= -1996 then
    raise Program_Error;
  end if;
  Data := To_Unsigned_Type (Temp);
  if Data /= 2100 then
    raise Program_Error;
  end if;

  Data := 2047;
  Temp := To_Signed_Type (Data);
  if Temp /= 2047 then
    raise Program_Error;
  end if;
  Data := To_Unsigned_Type (Temp);
  if Data /= 2047 then
    raise Program_Error;
  end if;

end;

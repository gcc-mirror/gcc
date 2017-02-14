-- { dg-do run }

with Unchecked_Conversion;

procedure Unchecked_Convert11 is

  subtype Unsigned_Type is Integer range 2_034 .. 2_164;

  subtype Signed_Type is Integer range -2048 .. 2047; 

  type Rec is record
    S : Signed_Type;
  end record;
  pragma Pack (Rec);

  function To_Signed_Type is
     new Unchecked_Conversion (Source => Unsigned_Type, Target => Rec);

  function To_Unsigned_Type is
     new Unchecked_Conversion (Source => Rec, Target => Unsigned_Type);

  Data : Unsigned_Type;
  Temp : Rec;

begin

  Data := 2100;
  Temp := To_Signed_Type (Data);
  if Temp.S /= -1996 then
    raise Program_Error;
  end if;
  Data := To_Unsigned_Type (Temp);
  if Data /= 2100 then
    raise Program_Error;
  end if;

  Data := 2047;
  Temp := To_Signed_Type (Data);
  if Temp.S /= 2047 then
    raise Program_Error;
  end if;
  Data := To_Unsigned_Type (Temp);
  if Data /= 2047 then
    raise Program_Error;
  end if;

end;

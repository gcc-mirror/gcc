-- { dg-do run }

procedure Volatile4 is

  type My_Int is new Integer;
  pragma Volatile (My_Int);

  type Rec is record
    I : My_Int;
  end record;

  function F (R : Rec) return Rec is
  begin
    return R;
  end;

  R : Rec := (I => 0);

begin
  R := F (R);
  if R.I /= 0 then
    raise Program_Error;
  end if;
end;

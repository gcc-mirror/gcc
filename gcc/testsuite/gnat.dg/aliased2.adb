-- { dg-do run }

procedure Aliased2 is

  type Rec is record
    Data : access constant String;
  end record;

  function Get (S : aliased String) return Rec is
    R : Rec := (Data => S'Unchecked_Access);
  begin
    return R;
  end;

  S : aliased String := "Hello";

  R : Rec := Get (S);

begin
  if R.Data'Length /= S'Length then
    raise Program_Error;
  end if;
end;

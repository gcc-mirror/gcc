-- { dg-do run }
-- Verify that an array of non-aliased zero-sized element is zero-sized

procedure Array10 is

  type Rec is null record;

  type Arr1 is array (1..8) of Rec;
  type Arr2 is array (Long_Integer) of Rec;

  R : Rec;
  A1 : Arr1;
  A2 : Arr2;

begin
  if Rec'Size /= 0 then
    raise Program_Error;
  end if;
  if Arr1'Size /= 0 then
    raise Program_Error;
  end if;
  if Arr2'Size /= 0 then
    raise Program_Error;
  end if;
end;

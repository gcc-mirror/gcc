-- { dg-do run }
-- { dg-options "-O2" }

-- The domain of an array with a negative lower bound has unsigned bounds
-- that look reversed.  Loads from it are not loads from an empty array.

procedure Opt111 is

  type Arr is array (-1 .. 5) of Integer;
  T : constant Arr := (11, 12, 13, 14, 15, 16, 17);

  function G (I : Integer; C : Boolean) return Integer;
  pragma No_Inline (G);

  function G (I : Integer; C : Boolean) return Integer is
    R : Integer := 0;
  begin
    if C then
      R := T (I);
    end if;
    return R;
  end;

begin
  if G (-1, True) /= 11 or else G (5, True) /= 17 then
    raise Program_Error;
  end if;
end;

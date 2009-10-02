-- { dg-do run }

procedure Array12 is

  function N return Integer is
  begin
    return 0;
  end;

  subtype Element is String (1 .. N);
  type Ptr is access all Element;
  type Vector is array (Positive range <>) of aliased Element;

  V : Vector (1..2);

begin
  if Ptr'(V(1)'Access) = V(2)'Access then
    raise Program_Error;
  end if;
end;

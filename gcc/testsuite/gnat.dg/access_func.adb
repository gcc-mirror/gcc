-- { dg-do compile }

procedure access_func is
    type Abomination is access
       function (X : Integer) return access
       function (Y : Float) return access
       function return Integer;
begin
    null;
end;

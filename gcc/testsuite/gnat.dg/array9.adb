-- { dg-do run }

procedure Array9 is

  V1 : String(1..10) := "1234567890";
  V2 : String(1..-1) := "";

  procedure Compare (S : String) is
  begin
    if S'Size /= 8*S'Length then
      raise Program_Error;
    end if;
  end;

begin
  Compare ("");
  Compare ("1234");
  Compare (V1);
  Compare (V2);
end;

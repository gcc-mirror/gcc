-- { dg-do compile }

with Array38_Pkg; use Array38_Pkg;

procedure Array38 is

  function My_F is new F (Index, Byte, Bytes, Integer);

begin
  null;
end;

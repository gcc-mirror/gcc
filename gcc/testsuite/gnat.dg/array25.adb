-- { dg-do compile }

with Array25_Pkg;

procedure Array25 is

   package My_Pkg is new Array25_Pkg (0, 0);

begin
   null;
end;

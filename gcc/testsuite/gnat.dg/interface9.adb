--  { dg-do compile }

with Interface9_Root.Child;
procedure Interface9 is
   package R   is new Interface9_Root (Real => Float);
   package RC  is new R.Child;

begin
   null;
end Interface9;

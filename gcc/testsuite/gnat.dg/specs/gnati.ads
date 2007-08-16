--  { dg-do compile }
--  { dg-options "-gnatI" }

package gnati is
   type j is range 1 .. 50;
   for j'size use 1;
   type n is new integer;
   for n'alignment use -99;
   type e is (a, b);
   for e use (1, 1);
   type r is record x : integer; end record;
   for r use record x at 0 range 0 .. 0; end record;
end gnati;

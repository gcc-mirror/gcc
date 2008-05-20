--  { dg-do run }
--  { dg-options "-gnato" }

with Text_IO; use Text_IO;
procedure testint is
   function m1 (a, b : short_integer) return integer is
   begin                          
      return integer (a + b);
   end m1;
   f : integer;
begin   
   f := m1 (short_integer'Last, short_integer'Last);
end testint;

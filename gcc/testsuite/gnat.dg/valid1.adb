--  { dg-do run }
--  { dg-options "-gnatVi" }

procedure valid1 is
   type m is range 0 .. 10;
   for m'size use 8;
   
   type r is record
      a, b : m;
      c, d, e, f : boolean;
   end record;
   pragma Pack (r);
   for R'size use 20;
   
   type G is array (1 .. 3, 1 .. 3) of R;
   pragma Pack (G);
   
   procedure h (c : m) is begin null; end;
   
   GG : G := (others => (others => (2, 3, true, true, true, true)));

begin
   h (GG (3, 2).a);
end;

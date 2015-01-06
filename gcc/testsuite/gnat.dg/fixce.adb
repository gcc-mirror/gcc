--  { dg-do run }
--  { dg-options "-gnatws" }

procedure fixce is
   type D is delta 128.0 / (2 ** 15) range 0.0 .. 256.0;
   type R is range 0 .. 200;
   dd : D;
   RA : constant array (1 .. 3) of R := (127, 128, 200);
begin
   dd := D (RA (2));
   for i in RA'range loop
      dd := D (RA (i));
   end loop;
end fixce;

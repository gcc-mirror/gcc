-- { dg-do compile }

with Ada.Text_IO; use Ada.Text_IO;

procedure Modular6 is
   Max : Integer := 0;
   
   type Modulus is mod 3;
begin
   Max := 30;
   
   for N in 1 .. Max loop
      Put_Line("N: " & Integer'Image(N) & " Modulus:    " & Integer'Image(Modulus'Modulus) & " Mod:" & Modulus'Image(Modulus'Mod(N)));
   end loop;
end;

-- { dg-do compile }
-- { dg-options "-O -gnatws" }

-- PR middle-end/35136

pragma Extend_System(AUX_DEC);
with System;

procedure Loop_Address is

   function Y(E : Integer) return String is
   begin
      return "";
   end Y;

   function X(C : in System.Address) return String is
      D : Integer;
      for D use at C;
   begin
      return Y(D);
   end X;

   A : System.Address;
   B : String := "";

begin
   for I in 0..1 loop
      B := X(System."+"(A, I));
   end loop;
end;

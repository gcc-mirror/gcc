--  { dg-do run }

with Ada.Exceptions; use Ada.Exceptions;

procedure Multfixed is
   Z : constant := 4387648782261400837.0;
   type F1 is delta 1.0 / Z range 0.0 .. (2.0**63-1.0) / Z
     with Small => 1.0 / Z;
   type F2 is delta 1.0 range 0.0 .. (2.0**63-1.0)
     with Small => 1.0;
   type D is delta 1.0 digits 18;

   X : F1 := 8914588002054909637.0 / Z;
   Y : F2 := 9079256848778919936.0;
   U : D;
begin
   U := D'Round(X * Y);
   raise Program_Error;
exception
   when Exc : Constraint_Error =>
      if Exception_Message (Exc) /= "System.Arith_64.Raise_Error: 64-bit arithmetic overflow" then
         raise Program_Error;
      end if;
end Multfixed;
--  { dg-do run }

with Ada.Text_IO;
procedure Modular1 is
   type T1 is mod 9;
   package T1_IO is new Ada.Text_IO.Modular_IO(T1);
   X: T1 := 8;
   J1: constant := 5;
begin                                                                              for J2 in 5..5 loop
      pragma Assert(X*(2**J1) = X*(2**J2));
      if X*(2**J1) /= X*(2**J2) then
         raise Program_Error;
      end if;
   end loop;
end Modular1;

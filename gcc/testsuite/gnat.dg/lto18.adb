-- { dg-do compile }
-- { dg-options "-flto" { target lto } }

package body Lto18 is

   procedure Proc (Driver : Rec) is
      R : Path;
   begin
      for I in Driver.Step'Range loop
         R := Get (Driver, 1, Driver.Step (I));
         R := Get (Driver, 2, Driver.Step (I));
         R := Get (Driver, 3, Driver.Step (I));
      end loop;
   end;

end Lto18;

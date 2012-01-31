-- { dg-do compile }
-- { dg-options "-O2 -gnatn" }

package body Opt24 is

   procedure Proc (Driver : Rec) is
      R : Path;
   begin
      for I in Driver.Step'Range loop
         R := Get (Driver, 1, Driver.Step (I));
         R := Get (Driver, 2, Driver.Step (I));
      end loop;
   end;

end Opt24;

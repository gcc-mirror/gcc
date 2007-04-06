-- { dg-do run }

   with System, NAT1; use NAT1;
   procedure Nat1R is
      use type System.Address;
   begin
      if One_Address /= Nat_One_Storage'Address then
         raise Constraint_Error;
      end if;
   end;


-- { dg-do run }

with System; use System;

procedure SSO3 is
   Rev_SSO : constant Bit_Order
     := Bit_Order'Val (1 - Bit_Order'Pos (Default_Bit_Order));

   type R (D : Integer) is record
      Common : Integer;
      case D is
         when 0 =>
            V1 : Integer;
         when others =>
            V2 : Integer;
      end case;
   end record;

   for R use record
      D at 0 range 0 .. 31;
      V1 at 4 range 0 .. 31;
      V2 at 4 range 0 .. 31;
      Common at 8 range 0 .. 31;
   end record;
   for R'Scalar_Storage_Order use Rev_SSO;
   for R'Bit_Order use Rev_SSO;

   procedure Check (Common, V : Integer; X : R) is
   begin
      if Common /= X.Common then
         raise Program_Error;
      end if;

      case X.D is
         when 0 =>
            if V /= X.V1 then
               raise Program_Error;
            end if;
         when others =>
            if V /= X.V2 then
               raise Program_Error;
            end if;
      end case;
   end Check;

   X0 : R := (D => 0,     Common => 1111, V1 => 1234);
   X1 : R := (D => 31337, Common => 2222, V2 => 5678);

begin
   Check (1111, 1234, X0);
   Check (2222, 5678, X1);
end;

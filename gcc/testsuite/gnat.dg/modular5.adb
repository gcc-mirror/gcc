-- { dg-do compile }
-- { dg-options "-gnata" }

procedure Modular5 is
   type U64 is mod 2 ** 64;
   Maybe    : Boolean := 2 ** 10 < U64'Succ (U64'last - 1);
   For_Sure : Boolean := U64'(18446744073709551615) > 2;
   Ditto    : Boolean := 18446744073709551615 > 2;

   generic
      type TG is mod <>;
   package PG is
     X : TG;
      pragma Assert (for all K in 1 .. 2 => 2 ** K <= TG'Last);
      pragma Assert (for all K in 1 .. 2 => 2 ** K <= TG'Last - 1);

     Maybe    : Boolean := 2 ** 10 < TG'Succ (TG'last - 1);
     For_Sure : Boolean := TG'(18446744073709551615) > 2;
   end PG;

   package IG is new PG (U64);

begin
   pragma Assert (for all K in 1 .. 2 => 2 ** K <= U64'Last);
   pragma Assert (for all K in 1 .. 2 => 2 ** K <= U64'Last - 1);
end Modular5;

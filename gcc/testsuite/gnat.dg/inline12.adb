-- PR ada/69219
-- Testcae by yuta tomino <demoonlit@panathenaia.halfmoon.jp> */

-- { dg-do compile }

procedure Inline12 is

   procedure NI;

   procedure IA;
   pragma Convention (Intrinsic, IA);
   pragma Inline_Always (IA);

   procedure IA is
   begin
      NI;
   end;

   procedure NI is null;

begin
  IA;
end;

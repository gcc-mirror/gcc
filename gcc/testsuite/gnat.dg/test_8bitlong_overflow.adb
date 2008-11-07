-- { dg-do run }
-- { dg-options "-gnato" }

procedure Test_8bitlong_Overflow is

   pragma Unsuppress (Overflow_Check);
   generic
       type T is range <>;
   package G is
      LO   : T := T'first;
      ONE  : T := T(1);

      type A2 is array(T range <>) of T;
      subtype SA2 is A2(LO..4*ONE);

      ARRAY_AGGR : SA2 := SA2'(others=>LO + 1);

      POS_1   : T := T'pos(LO*ONE);
   end;

   type T is new LONG_INTEGER range -1..10;
   for T'size use 8;

   package P is new G (T);

begin
   null;
end;

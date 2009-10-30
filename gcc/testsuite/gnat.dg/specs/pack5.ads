package Pack5 is

   type Small is range -32 .. 31;

   type Arr is array (Integer range <>) of Small;
   pragma Pack (Arr);

   type Rec is record
      Y: Arr (1 .. 10);
    end record;
   pragma Pack (Rec);

end Pack5;

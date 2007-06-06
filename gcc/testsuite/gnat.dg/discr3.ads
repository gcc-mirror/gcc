package discr3 is
   type E  is range  0..255;
   type R1 is range  1..5;
   type R2 is range 11..15;
   type S1 is array(R1 range <>) of E;
   type S2 is array(R2 range <>) of E;
   V1 : S1( 2..3)  := (0,0);
   V2 : S2(12..13) := (1,1);
   subtype R3 is R1 range 2..3;
   V3 : S1 (R3);
end discr3;

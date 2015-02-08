package Addr7 is

   type Bytes is array (1 .. 4) of Character;
   for Bytes'Alignment use 4;

   procedure Proc (B: aliased Bytes);

end Addr7;

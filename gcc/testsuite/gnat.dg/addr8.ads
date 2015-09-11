package Addr8 is

   type Bytes is array (1 .. 4) of Character;
   for Bytes'Alignment use 4;

   procedure Proc (B: Bytes);

end Addr8;

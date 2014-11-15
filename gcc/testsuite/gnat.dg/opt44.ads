package Opt44 is

   type Sarray is array (1 .. 4) of Float;
   for Sarray'Alignment use 16;

   procedure Addsub (X, Y : Sarray; R : out Sarray; N : Integer);

end Opt44;

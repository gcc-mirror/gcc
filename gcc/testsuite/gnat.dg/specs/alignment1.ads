-- { dg-do compile }

package Alignment1 is
   S : Natural := 20;
   pragma Volatile (S);

   type Block is array (1 .. S) of Integer;
   for Block'Alignment use 128;

   B : Block;
end;

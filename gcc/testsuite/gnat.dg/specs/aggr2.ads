-- { dg-do compile }

package Aggr2 is

   type Buffer is array (Positive range <>) of Boolean;
   for Buffer'Alignment use 4;

   type Buffer_Ptr is access Buffer;

   subtype My_Buffer is Buffer (1 .. 2);

   P : Buffer_Ptr := new My_Buffer'(Others => False);

end Aggr2;

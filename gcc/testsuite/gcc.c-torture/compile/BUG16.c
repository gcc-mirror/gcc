/* { dg-additional-options "-std=gnu89" } */

setgetlen (a)
     int *a;
{
  while (*a++ & 0x80000000)
    ;
}

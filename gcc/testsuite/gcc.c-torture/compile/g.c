/* { dg-additional-options "-std=gnu89" } */

foo (a, b)
     long long a, b;

{
  if (a == b)
    return 0;
  else
    return 1;
}

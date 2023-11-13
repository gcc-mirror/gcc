/* { dg-additional-options "-std=gnu89" } */

foo (a, b)
     long long a, b;
{
  if (a & ~b)
    return 1;
  else
    return 0;
}

bar (a, b)
     long long a, b;
{
  if (a & ~b & ((long long) 87612378))
    return 1;
  else
    return 0;
}

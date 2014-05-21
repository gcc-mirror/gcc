/* { dg-do run } */
/* { dg-options "-O2" } */

int a[1] = { 1 }, b = 1, c; 

int
main ()
{
  for (; c < 1; c++)
    if (a[0])
    {
      a[0] &= 1;
      b = 0;
    }
  if (b)
    __builtin_abort ();
  return 0;
}

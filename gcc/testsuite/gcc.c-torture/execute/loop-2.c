/* { dg-additional-options "-std=gnu89" } */

int a[2];

f (b)
{
  unsigned int i;
  for (i = 0; i < b; i++)
    a[i] = i - 2;
}

main ()
{
  a[0] = a[1] = 0;
  f (2);
  if (a[0] != -2 || a[1] != -1)
    abort ();
  exit (0);
}

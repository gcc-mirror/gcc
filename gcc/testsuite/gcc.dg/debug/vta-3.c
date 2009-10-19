/* { dg-do compile } */

int
foo (void)
{
  union { char e[8]; int i; } a, b;
  char *c, *d;
  unsigned int i;
  c = a.e;
  d = &b.e[sizeof (int) - 1];
  for (i = 0; i < sizeof (int); i++)
    {
      *d = *c++;
      --d;
    }
  return b.i;
}

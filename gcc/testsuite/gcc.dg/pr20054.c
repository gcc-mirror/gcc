
/* { dg-do compile } */
/* { dg-options "-O2 -fno-strict-aliasing" } */


unsigned int *foo (void);

char *
bar (double *d)
{
  return (char *) (d + 1) - sizeof (unsigned int);
}

char
baz (double x)
{
  unsigned int h = *foo ();
  unsigned int l = *(unsigned int *) bar (&x);

  return (h & ~0x80000000L) == 0x7FF00000 && l == 0;
}



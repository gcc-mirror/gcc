extern void abort (void);

int
main ()
{
  int i, j;
  unsigned long u, r1, r2;

  i = -16;
  j = 1;
  u = i + j;

  /* no sign extension upon shift */
  r1 = u >> 1;
  /* sign extension upon shift, but there shouldn't be */
  r2 = ((unsigned long) (i + j)) >> 1;

  if (r1 != r2)
    abort ();

  return 0;
}

/* { dg-do run } */
/* { dg-options "-O2" } */

/* The initializer leaves out the elements at 0, 1, 3 and 4, and those
   from 6 on: all read as zero, so whatever the index the load is 0, 5
   or 9.  */

void link_error (void);

static const int tbl[8] = { [2] = 5, [5] = 9 };

[[gnu::noipa]] int
foo (int i)
{
  int v = tbl[i];
  if (v != 0 && v != 5 && v != 9)
    link_error ();
  return v == 0;
}

int
main (void)
{
  if (!foo (0) || foo (2) || foo (5) || !foo (7))
    __builtin_abort ();
  return 0;
}

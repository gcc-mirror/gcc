/* { dg-do run } */
/* { dg-options "-O1 -floop-nest-optimize" } */

extern void abort (void);

int yu[4][1] = { { 1 }, { 2 }, { 3 }, { 4 } };

static void __attribute__((noinline,noclone))
foo (void)
{
  int zh, ro;

  for (zh = 0; zh < 2; ++zh)
    for (ro = 0; ro < 3; ++ro)
      yu[ro][0] = yu[zh + 1][0];
}

int
main (void)
{
  foo ();

  if (yu[0][0] != 2
      || yu[1][0] != 2
      || yu[2][0] != 2
      || yu[3][0] != 4)
    abort ();

  return 0;
}

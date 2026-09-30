/* { dg-do run } */
/* { dg-options "-O2" } */

/* The range of the load of sa[i].a under i < 2 is [1, 3].  Once it is
   hoisted above the condition, that range must not fold the later
   sa[i].a > 3.

   This is the testcase for r17-4029-g7dab38c9d7104e ("phiopt: clear the
   range info of hoisted adjacent loads"), which could not be exercised
   until now.  */

struct S { int a, b; };
static const struct S sa[4] = { { 1, 2 }, { 3, 4 }, { 5, 6 }, { 7, 8 } };
volatile int vol;

__attribute__((noipa)) int
f (int i)
{
  int r;
  if (i < 2)
    r = sa[i].a;
  else
    r = sa[i].b;
  /* Enough statements that the merge block is not worth threading.  */
  vol; vol; vol; vol; vol; vol; vol; vol;
  vol; vol; vol; vol; vol; vol; vol; vol;
  if (sa[i].a > 3)
    return 100 + r;
  return r;
}

int
main ()
{
  if (f (0) != 1 || f (1) != 3 || f (2) != 106 || f (3) != 108)
    __builtin_abort ();
  return 0;
}

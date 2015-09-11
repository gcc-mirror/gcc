/* { dg-do compile } */
/* { dg-options "-fcheck-pointer-bounds -mmpx -O -fvisibility=hidden" } */

int val;

static int __attribute__((noinline))
test1 ()
{
  return val;
}

static int __attribute__((bnd_legacy,noinline))
test2 ()
{
  return test1 ();
}

int
test3 (void)
{
  return test2 ();
}


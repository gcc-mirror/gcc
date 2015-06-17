/* { dg-lto-do link } */
/* { dg-require-effective-target mpx } */
/* { dg-lto-options { { -Ofast -flto -fcheck-pointer-bounds -mmpx } } } */

static int
__attribute__ ((noinline))
func1 (int i)
{
  return i + 2;
}

extern int func2 (int i);

int
main (int argc, char **argv)
{
  return func1 (argc) + func2 (argc) + 1;
}

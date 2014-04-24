/* { dg-do run } */
/* { dg-options "-fuse-caller-save" } */
/* Testing -fuse-caller-save optimization option.  */

static int __attribute__((noinline))
bar (int x)
{
  return x + 3;
}

int __attribute__((noinline))
foo (int y)
{
  return y + bar (y);
}

int
main (void)
{
  return !(foo (5) == 13);
}

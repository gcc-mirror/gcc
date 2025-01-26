/* { dg-do run } */
/* { dg-options "-O2" } */

register long x asm ("s0");

#define TEST(x) (int)(((x & 0x114) << 3) + x)

[[gnu::noipa]] void
test (void)
{
  x = TEST (x);
}

int
main (void)
{
  x = 0xffff;
  test ();
  if (x != TEST (0xffff))
    __builtin_trap ();
}

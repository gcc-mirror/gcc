/* { dg-do run } */
/* { dg-options "-O2" } */

int g1 = 1, g2 = 2;

#define TEST(NAME, OP)					\
  __attribute__ ((noinline, noclone)) int		\
  NAME (short *p, int a, int b)				\
  {							\
    *p OP 0x10;						\
    if (a == b)						\
      return g1;					\
    return g2;						\
  }

TEST (f_set1, |=)
TEST (f_not1, ^=)

int
main (void)
{
  short x = 0;

  if (f_set1 (&x, 1, 2) != 2)
    __builtin_abort ();
  x = 0;
  if (f_not1 (&x, 1, 2) != 2)
    __builtin_abort ();
  __builtin_exit (0);
}

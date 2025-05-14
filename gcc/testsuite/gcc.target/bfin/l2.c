/* { dg-do run { target bfin-*-linux-uclibc } } */
/* { dg-bfin-processors bf544 bf547 bf548 bf549 bf561 } */

#if defined(__ADSPBF544__)
#define L2_START 0xFEB00000
#define L2_LENGTH 0x10000
#else
#define L2_START 0xFEB00000
#define L2_LENGTH 0x20000
#endif

int n __attribute__ ((l2));

int foo (int i) __attribute__ ((l2));

int foo (int a)
{
  return a + 1;
}

int main ()
{
  int r;
  unsigned long *p;

  p = (unsigned long *) foo;
  if (*p < L2_START || *p >= L2_START + L2_LENGTH)
    return 1;

  p = (unsigned long *) &n;
  if ((unsigned long) p < L2_START || (unsigned long) p >= L2_START + L2_LENGTH)
    return 2;

  if (foo (0) != 1)
    return 3;

  return 0;
}

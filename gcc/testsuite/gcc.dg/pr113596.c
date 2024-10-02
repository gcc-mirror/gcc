/* PR middle-end/113596 */
/* { dg-do run } */
/* { dg-options "-O2" } */

__attribute__((noipa)) void
bar (char *p, int n)
{
  p[0] = 1;
  p[n - 1] = 2;
}

static inline __attribute__((always_inline)) void
foo (int n)
{
  char *p = __builtin_alloca (n);
  bar (p, n);
}

#if defined __AVR__
/* For AVR devices, AVRtest assigns 8 KiB of stack, which is not quite
   enough for this test case.  Thus request less memory on AVR.  */
#define ALLOC 6000
#else
#define ALLOC 8192
#endif

int
main ()
{
  for (int i = 2; i < ALLOC; ++i)
    foo (i);
}

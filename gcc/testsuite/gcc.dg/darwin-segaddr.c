/* Check that -segaddr gets through and works.  */
/* { dg-do run { target *-*-darwin* } } */
/* { dg-options "-O0 -segaddr __TEST 0x200000" { target { *-*-darwin* && { ! lp64 } } } } */
/* { dg-options "-O0 -segaddr __TEST 0x110000000" { target { *-*-darwin* && lp64 } } } */

extern void abort ();

int t __attribute__((section("__TEST,__test")));

int main (void)
{
#ifdef __LP64__
  if ((unsigned long long) &t != 0x110000000ULL)
#else
  if ((unsigned long) &t != 0x200000UL)
#endif
    abort ();
  return 0;
}

/* Check that the XF registers are not clobbered by an integer division
   that is done using double precision FPU division.  */
/* { dg-do run { target "sh*-*-*" } }  */
/* { dg-options "-O1 -mdiv=call-fp" }  */
/* { dg-skip-if "" { "sh*-*-*" } { "*" } { "-m4*-single" "-m4*-single-only" } }  */

#include <assert.h>
#include <stdlib.h>

extern void __set_fpscr (int);

void
write_xf0 (float* f)
{
  __asm__ __volatile__ ("frchg; fmov.s @%0,fr0; frchg" : : "r" (f) : "memory");
}
 
void
read_xf0 (float* f)
{
  __asm__ __volatile__ ("frchg; fmov.s fr0,@%0; frchg" : : "r" (f) : "memory");
}

int __attribute__ ((noinline))
test_00 (int a, int b)
{
  return a / b;
}

unsigned int __attribute__ ((noinline))
test_01 (unsigned a, unsigned b)
{
  return a / b;
}

int __attribute__ ((noinline))
test_02 (int x)
{
  return x & 0;
}

int
main (void)
{
  float test_value;
  int r = 0;

  /* Set FPSCR.FR to 1.  */
  __set_fpscr (0x200000);

  test_value = 123;
  write_xf0 (&test_value);
  r += test_00 (40, 4);
  read_xf0 (&test_value);
  assert (test_value == 123);

  test_value = 321;
  write_xf0 (&test_value);
  r += test_01 (50, 5);
  read_xf0 (&test_value);
  assert (test_value == 321);

  return test_02 (r);
}

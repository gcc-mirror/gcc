/* { dg-do compile { target { ! x32 } } } */
/* { dg-require-alias "" } */
/* { dg-options "-fcheck-pointer-bounds -mmpx -O2" } */
/* { dg-final { scan-assembler-not "test.chkp" } } */

int test (int *p);
extern __typeof (test) test  __asm__ ("" "__test") __attribute__ ((visibility ("hidden")));

int test (int *p)
{
  return *p;
}

extern __typeof (test) __test1 __asm__("" "test");
extern __typeof (test) __test1 __attribute__((alias ("" "__test")));

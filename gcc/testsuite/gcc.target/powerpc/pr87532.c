/* { dg-do run } */
/* { dg-require-effective-target vmx_hw } */
/* { dg-options "-maltivec" } */

/* This test should run the same on any target that supports altivec/dfp
   instructions.  Intentionally not specifying cpu in order to test
   all code generation paths.  */

#include <stdlib.h>
#include <stddef.h>
#include <altivec.h>
#include <stdio.h>

static void
check (unsigned char, unsigned char) __attribute__((__noinline__));

static __attribute__((altivec(vector__))) unsigned char
deoptimize (__attribute__((altivec(vector__))) unsigned char)
__attribute__((__noinline__));

static __attribute__((altivec(vector__))) unsigned char
deoptimize (__attribute__((altivec(vector__))) unsigned char a)
{
  __asm__ (" # %x0" : "+v" (a));
  return a;
}

// Toggle this attribute inline/noinline to see pass/fail.
// fails with the noinline attribute applied.
__attribute__ ((__noinline__))
unsigned char
get_auto_n (__attribute__((altivec(vector__))) unsigned char a, size_t n)
{
  return (unsigned char) __builtin_vec_extract (a, n);
}

void
do_auto (__attribute__((altivec(vector__))) unsigned char a)
{
  size_t i;
  for (i = 1; i < 3 ; i++)
  {
    do
      {
	printf ("get_auto_n (a, %d) produces 0x0%x\n",
		i, (int) get_auto_n (a, i));

	if ((int) get_auto_n (a,i) > 250) abort();
      } while (0);
  }
}

int
main (void)
{
  size_t i;
  __attribute__((altivec(vector__))) unsigned char x =
    { 3, 2, 3, 8, 5, 6, 7, 8, 240, 241, 242, 243, 244, 245, 246, 247 };
  __attribute__((altivec(vector__))) unsigned char a;

  printf (" first elements of x are: %d %d %d %d %d\n",
	  x[0], x[1], x[2], x[3], x[4]);

  a = deoptimize (x);

  printf (" after deoptimization, first elements of a are: %d %d %d %d %d\n",
	  a[0], a[1], a[2], a[3], a[4]);

  do_auto (a);

  return 0;
}

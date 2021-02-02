/* { dg-require-effective-target ppc_float128_hw } */
/* { dg-options "-mdejagnu-cpu=power9 -O2 -mlong-double-128 -Wno-psabi -mabi=ieeelongdouble" } */

/* Test if switching long double to IEEE 128-bit maps the printf and scanf
   function names correctly.  We leave off the \M in matching the calls, so
   power10 will match using bl foo@notoc.  */

#include <stdlib.h>

volatile long double x = 1.0L;
volatile long double y, z;

int
main (void)
{
  char buffer[100];

  /* { dg-final { scan-assembler {\mbl __sprintfieee128} } }  */
  __builtin_sprintf (buffer, "%Lg", x);

  /* { dg-final { scan-assembler {\mbl __printfieee128} } }  */
  __builtin_printf ("x is %Lg [%s]\n", x, buffer);

  /* { dg-final { scan-assembler {\mbl __isoc99_sscanfieee128} } }  */
  __builtin_sscanf (buffer, "%Lg", &y);

  __builtin_printf ("Type 1.0: ");

  /* { dg-final { scan-assembler {\mbl __isoc99_scanfieee128} } }  */
  __builtin_scanf ("%Lg", &z);

  if (x != y || x != z)
    abort ();

  return 0;
}

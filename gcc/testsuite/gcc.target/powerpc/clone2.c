/* { dg-do run { target { powerpc*-*-linux* } } } */
/* { dg-options "-mvsx -O2" } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-require-effective-target ppc_cpu_supports_hw } */

#include <stddef.h>
#include <stdlib.h>

/* Power9 (aka, ISA 3.0) has a MODSD instruction to do modulus, while Power8
   (aka, ISA 2.07) has to do modulus with divide and multiply.  Make sure that
   the basic support for target_clones runs.

   Restrict ourselves to Linux, since IFUNC might not be supported in other
   operating systems.  */

__attribute__((__target_clones__("cpu=power9,default")))
long mod_func (long a, long b)
{
  return a % b;
}

#define X 53L
#define Y 7L
int
main (void)
{
  if (mod_func (X, Y) != (X % Y))
    abort ();

  return 0;
}

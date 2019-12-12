/* Verify that overloaded built-ins for vec_cmp with int
   inputs produce the right code.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mpower8-vector -mdejagnu-cpu=power8 -O2" } */

#include "fold-vec-cmp-int.h"

/* { dg-final { scan-assembler-times "vcmpequw" 4 } } */
/* { dg-final { scan-assembler-times "vcmpgtsw" 4 } } */
/* { dg-final { scan-assembler-times "vcmpgtuw" 4 } } */
/* { dg-final { scan-assembler-times "xxlnor" 6 } } */

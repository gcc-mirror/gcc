/* Verify that overloaded built-ins for vec_cmp with short
   inputs produce the right code.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mpower8-vector -mdejagnu-cpu=power8 -O2" } */

#include "fold-vec-cmp-short.h"

/* { dg-final { scan-assembler-times "vcmpequh" 4 } } */
/* { dg-final { scan-assembler-times "vcmpneh" 0 } } */
/* { dg-final { scan-assembler-times "vcmpgtsh" 4 } } */
/* { dg-final { scan-assembler-times "vcmpgtuh" 4 } } */
/* { dg-final { scan-assembler-times "xxlnor" 6 } } */


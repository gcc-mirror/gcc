/* Verify that overloaded built-ins for vec_cmp with short
   inputs produce the right code.  */

/* { dg-do compile } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx -O2" } */
/* { dg-require-effective-target powerpc_vsx } */

#include "fold-vec-cmp-short.h"

/* { dg-final { scan-assembler-times "vcmpequh" 2 } } */
/* { dg-final { scan-assembler-times "vcmpneh"  2 } } */
/* { dg-final { scan-assembler-times "vcmpgtsh" 4 } } */
/* { dg-final { scan-assembler-times "vcmpgtuh" 4 } } */
/* { dg-final { scan-assembler-times "xxlnor" 4 } } */


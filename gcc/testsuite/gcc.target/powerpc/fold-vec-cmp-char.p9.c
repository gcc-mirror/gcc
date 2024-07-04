/* Verify that overloaded built-ins for vec_cmp{eq,ge,gt,le,lt,ne} with
   char inputs produce the right code when -mcpu=power9 is specified.  */

/* { dg-do compile } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx  -O2" } */
/* { dg-require-effective-target powerpc_vsx } */

#include "fold-vec-cmp-char.h"

/* { dg-final { scan-assembler-times "vcmpneb" 2 } } */
/* { dg-final { scan-assembler-times "vcmpequb" 2 } } */
/* { dg-final { scan-assembler-times "vcmpgtsb" 4 } } */
/* { dg-final { scan-assembler-times "vcmpgtub" 4 } } */
/* { dg-final { scan-assembler-times "xxlnor" 4 } } */


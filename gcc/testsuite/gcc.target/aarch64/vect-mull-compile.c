
/* { dg-do compile } */
/* { dg-options "-O3" } */

#define N 16

#include "vect-mull.x"

DEF_MULL2 (DEF_MULLB)
DEF_MULL2 (DEF_MULLH)
DEF_MULL2 (DEF_MULLS)

/* { dg-final { scan-assembler-times "smull v" 3 } } */
/* { dg-final { scan-assembler-times "smull2 v" 3 } } */
/* { dg-final { scan-assembler-times "umull v" 3 } } */
/* { dg-final { scan-assembler-times "umull2 v" 3 } } */

/* { dg-options "-O2 -ftree-vectorize" } */

#include "pr99929_1.c"

/* { dg-final { scan-assembler {\tptrue\tp[0-7].[bhsd], vl1\n} } } */

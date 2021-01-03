/* { dg-do compile } */
/* { dg-options "-O3 -march=z14 -mzarch -mvx-long-double-fma" } */
#include "long-double-wf.h"

LONG_DOUBLE_WF (NEG_MUL_SUB);

/* { dg-final { scan-assembler {\n\twfnmsxb\t} } } */

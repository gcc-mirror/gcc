/* { dg-do compile } */
/* { dg-options "-O3 -march=z14 -mzarch -mvx-long-double-fma" } */
#include "long-double-wf.h"

LONG_DOUBLE_WF (NEG_MUL_ADD);

/* { dg-final { scan-assembler {\n\twfnmaxb\t} } } */

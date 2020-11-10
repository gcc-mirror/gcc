/* { dg-do compile } */
/* { dg-options "-O3 -march=z14 -mzarch" } */
#include "long-double-wf.h"

LONG_DOUBLE_WF (MUL);

/* { dg-final { scan-assembler {\n\twfmxb\t} } } */

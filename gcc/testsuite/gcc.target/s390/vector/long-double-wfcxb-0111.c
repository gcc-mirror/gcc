/* { dg-do compile } */
/* { dg-options "-O3 -march=z14 -mzarch" } */
#include "long-double-wf.h"

LONG_DOUBLE_WF (QUIET_IFEQUAL);

/* { dg-final { scan-assembler {\n\twfcxb\t} } } */
/* jne == brc 0b0111, ...  */
/* { dg-final { scan-assembler {\n\tjne\t} } } */

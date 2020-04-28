/* { dg-do compile } */
/* { dg-options "-O3 -march=z14 -mzarch" } */
#include "long-double-wf.h"

LONG_DOUBLE_WF (QUIET_IFUNORDERED);

/* { dg-final { scan-assembler {\n\twfcxb\t} } } */
/* jo == brc 0b0001, ...  */
/* { dg-final { scan-assembler {\n\tjo\t} } } */

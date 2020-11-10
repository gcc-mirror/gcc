/* { dg-do compile } */
/* { dg-options "-O3 -march=z14 -mzarch" } */
#include "long-double-wf.h"

LONG_DOUBLE_WF (SIGNALING_IFLESS);

/* { dg-final { scan-assembler {\n\twfkxb\t} } } */
/* jnl == brc 0b1011, ...  */
/* { dg-final { scan-assembler {\n\tjnl\t} } } */

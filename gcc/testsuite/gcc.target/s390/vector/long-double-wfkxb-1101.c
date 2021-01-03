/* { dg-do compile } */
/* { dg-options "-O3 -march=z14 -mzarch" } */
#include "long-double-wf.h"

LONG_DOUBLE_WF (SIGNALING_IFGREATER);

/* { dg-final { scan-assembler {\n\twfkxb\t} } } */
/* jnh == brc 0b1101, ...  */
/* { dg-final { scan-assembler {\n\tjnh\t} } } */

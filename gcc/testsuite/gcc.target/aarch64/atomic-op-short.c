/* { dg-do compile } */
/* { dg-options "-O2" } */

#include "atomic-op-short.x"

/* { dg-final { scan-assembler-times "ldxrh\tw\[0-9\]+, \\\[x\[0-9\]+\\\]" 6 } } */
/* { dg-final { scan-assembler-times "stxrh\tw\[0-9\]+, w\[0-9\]+, \\\[x\[0-9\]+\\\]" 6 } } */

/* { dg-do compile } */
/* { dg-options "-O2 -mcpu=olympus --param=max-rtl-if-conversion-unpredictable-cost=1 --param=max-rtl-if-conversion-predictable-cost=1 -fdump-rtl-ce1" } */

#include "noce-parallel-cost-1.c"

/* { dg-final { scan-rtl-dump-not "if-conversion succeeded" "ce1" } } */

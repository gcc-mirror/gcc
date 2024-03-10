/* { dg-do compile } */
/* { dg-options "-O -fno-tree-ch --param=max-completely-peel-times=0 -march=rv64iv -mabi=lp64d -fno-dce -fschedule-insns -fcompare-debug" } */
#include "compare-debug-1.c"

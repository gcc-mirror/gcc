/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -march=x86-64-v4 -fomit-frame-pointer" } */
/* { dg-final { scan-assembler "and\[lq\]?\[\\t \]*\\$-64,\[\\t \]*%\[re\]?sp" } } */

#define N 16

#include "pr126410-1a.c"

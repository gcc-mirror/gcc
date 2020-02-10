/* Check that eliminable compare-instructions are eliminated. */
/* { dg-do compile } */
/* { dg-skip-if "" { "*-*-*" } { "-march=*" } { "" } } */
/* { dg-options "-O2 -march=v10" } */
/* { dg-final { scan-assembler-not "\tcmp|\ttest" } } */
/* { dg-final { scan-assembler "\tswapwb " } } */

#define op(xx) __builtin_bswap32(xx)

#include "pr93372-31.c"

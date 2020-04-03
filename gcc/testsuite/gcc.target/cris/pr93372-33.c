/* Check that eliminable compare-instructions are eliminated. */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not "\tcmp|\ttest" } } */
/* { dg-final { scan-assembler "\tbound" } } */

#define t unsigned int
#define op(xx) ((xx) < x ? (xx) : x)

#include "pr93372-31.c"

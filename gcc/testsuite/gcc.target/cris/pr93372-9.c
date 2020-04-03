/* Check that eliminable compare-instructions are eliminated. */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-times "\tcmp" 1 } } */
/* { dg-final { scan-assembler-not "\ttest" } } */

#define t short int
#include "pr93372-3.c"
#include "pr93372-7.c"

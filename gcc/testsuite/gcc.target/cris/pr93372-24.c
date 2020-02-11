/* Check that eliminable compare-instructions are eliminated. */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not "\tcmp|\ttest" } } */
/* For the f2, we want an andq, not and.b. */
/* { dg-final { scan-assembler "\tandq 20," } } */
/* { dg-final { scan-assembler-not "\tand.b 20," } } */
/* See pr93372-23.c regarding the xfail.  */
/* { dg-final { scan-assembler-not "\tmove.b" { xfail cc0 } } } */

#define op &
#define t signed char
#include "pr93372-16.c"

#undef op
#define op & 20 &
#define f f2
#define g g2
#include "pr93372-16.c"

/* Check that eliminable compare-instructions are eliminated. */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not "\tcmp|\ttest" } } */
/* For the f2, we want an andq, not and.w. */
/* { dg-final { scan-assembler "\tandq 20," } } */
/* { dg-final { scan-assembler-not "\tand.w 20," } } */
/* We don't want a move.w that sets condition codes, but it happens for
   cc0, as the "andq" that is the last insn before the branch, is for an
   alternative that matches -32..31 and thus marked as clobbering
   condition codes.  */
/* { dg-final { scan-assembler-not "\tmove.w" { xfail cc0 } } } */

#define op &
#include "pr93372-16.c"

#undef op
#define op & 20 &
#define f f2
#define g g2
#include "pr93372-16.c"

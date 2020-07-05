/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not {\tcmp|\tsub\.|\tadd\.|\tmovu|\tmovs} } } */
/* { dg-final { scan-assembler-times "\ttest\.w" 4 } } */
/* { dg-final { scan-assembler-times "\tadds" 1 } } */
/* { dg-final { scan-assembler-times "\tsubs" 1 } } */
/* { dg-final { scan-assembler-times "\taddu" 1 } } */
/* { dg-final { scan-assembler-times "\tsubu" 1 } } */

/* Check that we produce sign- and zero-extended additions and
   subtractions, also for 8-bit to 16-bit results.  Note that we can't
   eliminate compare insns, as the condition codes reflect the 32-bit
   result.
   This test-case is brittle, as with the presence of compare
   instructions, there are several optimal instruction sequence, some of
   which match the non-matcher patterns and do not contain the matching
   patterns. */

#define t unsigned char
#define t2 unsigned short
#define s _us
#include "pr93372-36.c"

#undef t
#undef s
#undef t2
#define t signed char
#define t2 signed short
#define s _ss
#include "pr93372-36.c"

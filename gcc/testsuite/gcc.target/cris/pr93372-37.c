/* Check that we produce sign- and zero-extended additions and
   subtractions, and that no (eliminable) test- or compare-instructions
   are used. */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not {\tcmp|\ttest|\tsub\.|\tadd\.|\tmovu|\tmovs} } } */
/* { dg-final { scan-assembler-times "\tadds" 1 } } */
/* { dg-final { scan-assembler-times "\tsubs" 1 } } */
/* { dg-final { scan-assembler-times "\taddu" 2 } } */
/* { dg-final { scan-assembler-times "\tsubu" 2 } } */

#define t unsigned char
#define s _uc
#include "pr93372-36.c"

#undef t
#undef s
#define t signed short int
#define s _ss
#include "pr93372-36.c"

#undef t
#undef s
#define t unsigned short int
#define s _us
#include "pr93372-36.c"

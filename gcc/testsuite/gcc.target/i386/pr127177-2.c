/* PR target/127177 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx512vl -ftrapping-math" } */

#include "pr127177-1.c"

/* { dg-final { scan-assembler "vcmpps\[ \t\]*\\\$21," } } */
/* { dg-final { scan-assembler "vcmpps\[ \t\]*\\\$22," } } */
/* { dg-final { scan-assembler "vcmpps\[ \t\]*\\\$25," } } */
/* { dg-final { scan-assembler "vcmpps\[ \t\]*\\\$26," } } */
/* { dg-final { scan-assembler "vcmpps\[ \t\]*\\\$8," } } */
/* { dg-final { scan-assembler-not "vcmpps\[ \t\]*\\\$\[569\]," } } */
/* { dg-final { scan-assembler-not "vcmpps\[ \t\]*\\\$1\[08\]," } } */

/* { dg-do compile } */
/* { dg-options "-march=cascadelake -O3" } */
/* { dg-final { scan-assembler-not "kunpck" } } */
/* { dg-final { scan-assembler-not "kand" } } */
/* { dg-final { scan-assembler-not "kor" } } */
/* { dg-final { scan-assembler-not "kshift" } } */

#include "pr103771.c"

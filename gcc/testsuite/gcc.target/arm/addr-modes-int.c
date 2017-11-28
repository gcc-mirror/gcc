/* { dg-options "-O2 -march=armv7-a" } */
/* { dg-add-options arm_neon } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-do compile } */

#include "addr-modes.h"

typedef long long ll;

PRE_STORE(char)
/* { dg-final { scan-assembler "strb.*#1]!" } } */
PRE_STORE(short)
/* { dg-final { scan-assembler "strh.*#2]!" } } */
PRE_STORE(int)
/* { dg-final { scan-assembler "str.*#4]!" } } */
PRE_STORE(ll)
/* { dg-final { scan-assembler "strd.*#8]!" } } */

POST_STORE(char)
/* { dg-final { scan-assembler "strb.*], #1" } } */
POST_STORE(short)
/* { dg-final { scan-assembler "strh.*], #2" } } */
POST_STORE(int)
/* { dg-final { scan-assembler "str.*], #4" } } */
POST_STORE(ll)
/* { dg-final { scan-assembler "strd.*], #8" } } */

PRE_LOAD(char)
/* { dg-final { scan-assembler "ldrb.*#1]!" } } */
PRE_LOAD(short)
/* { dg-final { scan-assembler "ldrsh.*#2]!" } } */
PRE_LOAD(int)
/* { dg-final { scan-assembler "ldr.*#4]!" } } */
PRE_LOAD(ll)
/* { dg-final { scan-assembler "ldrd.*#8]!" } } */

POST_LOAD(char)
/* { dg-final { scan-assembler "ldrb.*], #1" } } */
POST_LOAD(short)
/* { dg-final { scan-assembler "ldrsh.*], #2" } } */
POST_LOAD(int)
/* { dg-final { scan-assembler "ldr.*], #4" } } */
POST_LOAD(ll)
/* { dg-final { scan-assembler "ldrd.*], #8" } } */

/* { dg-final { scan-assembler-not "\tadd" } } */

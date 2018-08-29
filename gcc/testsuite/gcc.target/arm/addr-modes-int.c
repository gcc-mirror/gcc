/* { dg-do compile } */
/* { dg-options "-O2 -march=armv7-a" } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-add-options arm_neon } */

#include "addr-modes.h"

typedef long long ll;

/* no special function attribute required */
#define ATTR /* */

PRE_STORE(char, ATTR)
/* { dg-final { scan-assembler "strb.*#1]!" } } */
PRE_STORE(short, ATTR)
/* { dg-final { scan-assembler "strh.*#2]!" } } */
PRE_STORE(int, ATTR)
/* { dg-final { scan-assembler "str.*#4]!" } } */
PRE_STORE(ll, ATTR)
/* { dg-final { scan-assembler "strd.*#8]!" } } */

POST_STORE(char, ATTR)
/* { dg-final { scan-assembler "strb.*], #1" } } */
POST_STORE(short, ATTR)
/* { dg-final { scan-assembler "strh.*], #2" } } */
POST_STORE(int, ATTR)
/* { dg-final { scan-assembler "str.*], #4" } } */
POST_STORE(ll, ATTR)
/* { dg-final { scan-assembler "strd.*], #8" } } */

PRE_LOAD(char, ATTR)
/* { dg-final { scan-assembler "ldrb.*#1]!" } } */
PRE_LOAD(short, ATTR)
/* { dg-final { scan-assembler "ldrsh.*#2]!" } } */
PRE_LOAD(int, ATTR)
/* { dg-final { scan-assembler "ldr.*#4]!" } } */
PRE_LOAD(ll, ATTR)
/* { dg-final { scan-assembler "ldrd.*#8]!" } } */

POST_LOAD(char, ATTR)
/* { dg-final { scan-assembler "ldrb.*], #1" } } */
POST_LOAD(short, ATTR)
/* { dg-final { scan-assembler "ldrsh.*], #2" } } */
POST_LOAD(int, ATTR)
/* { dg-final { scan-assembler "ldr.*], #4" } } */
POST_LOAD(ll, ATTR)
/* { dg-final { scan-assembler "ldrd.*], #8" } } */

/* { dg-final { scan-assembler-not "\tadd" } } */

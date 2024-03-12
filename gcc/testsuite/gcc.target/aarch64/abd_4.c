/* { dg-do compile } */
/* { dg-options "-O3" } */

#pragma GCC target "+nosve"
#define N 1024

#define ABD_IDIOM
#include "abd.h"

TEST1(signed, int)

TEST3(signed, int, char, int)
TEST3(signed, int, short, int)

/* { dg-final { scan-assembler-times "sabd\\tv\[0-9\]+\.4s, v\[0-9\]+\.4s, v\[0-9\]+\.4s" 7 } } */
/* { dg-final { scan-assembler-times "sabd\\tv\[0-9\]+\.8h, v\[0-9\]+\.8h, v\[0-9\]+\.8h" 0 } } */
/* { dg-final { scan-assembler-times "sabd\\tv\[0-9\]+\.16b, v\[0-9\]+\.16b, v\[0-9\]+\.16b" 0 } } */

/* { dg-final { scan-assembler-times "uabd\\tv\[0-9\]+\.4s, v\[0-9\]+\.4s, v\[0-9\]+\.4s" 0 } } */
/* { dg-final { scan-assembler-times "uabd\\tv\[0-9\]+\.8h, v\[0-9\]+\.8h, v\[0-9\]+\.8h" 0 } } */
/* { dg-final { scan-assembler-times "uabd\\tv\[0-9\]+\.16b, v\[0-9\]+\.16b, v\[0-9\]+\.16b" 0:w
 } } */

/* { dg-final { scan-assembler-not {\tsabdl\t} } } */
/* { dg-final { scan-assembler-not {\tsabdl2\t} } } */
/* { dg-final { scan-assembler-not {\tuabdl\t} } } */
/* { dg-final { scan-assembler-not {\tuabdl2\t} } } */
/* { dg-final { scan-assembler-not {\tabs\t} } } */

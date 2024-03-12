/* { dg-do compile } */
/* { dg-additional-options "-march=armv8-a+memtag" } */

#include <arm_acle.h>

int *bar (int *src) {
        return __arm_mte_create_random_tag(src, 2<<16-1);
}

/* { dg-final { scan-assembler {\tirg\t} } } */

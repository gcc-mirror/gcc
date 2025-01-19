/* PR target/112600 */
/* { dg-do compile } */
/* { dg-options "-O2 -msse2 -fdump-tree-optimized" } */

#include "pr112600-5.h"

DEF_SAT_ADD (uint32_t)

/* { dg-final { scan-tree-dump-times ".SAT_ADD " 1 "optimized" } } */

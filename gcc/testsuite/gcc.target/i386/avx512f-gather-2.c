/* { dg-do compile } */ /* PR59617 */
/* { dg-options "-O3 -mavx512f -fdump-tree-vect-details" } */

#include "avx512f-gather-1.c"

/* { dg-final { scan-assembler-not "gather\[^\n\]*ymm\[^\n\]*ymm" { xfail { *-*-* } } } } */  /* PR59617 */
/* { dg-final { scan-assembler-not "gather\[^\n\]*xmm\[^\n\]*ymm" { xfail { *-*-* } } } } */  /* PR59617 */
/* { dg-final { scan-assembler-not "gather\[^\n\]*ymm\[^\n\]*xmm" { xfail { *-*-* } } } } */  /* PR59617 */
/* { dg-final { scan-assembler-not "gather\[^\n\]*xmm\[^\n\]*xmm" { xfail { lp64 } } } } */  /* PR59617 */
/* { dg-final { scan-tree-dump-times "note: vectorized 1 loops in function" 16 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */

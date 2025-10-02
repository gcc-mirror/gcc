/* { dg-do compile } */
/* { dg-options "-O3 -fno-tree-dominator-opts -fno-tree-vrp -fno-tree-ccp -fno-tree-forwprop -fno-tree-pre -fno-tree-fre -mno-avx512f -mavx -mprefer-vector-width=256 -mmemset-strategy=rep_byte:8192:align,libcall:-1:noalign" } */

#include "pr121934-4a.c"

/* { dg-final { scan-assembler-not "rep stos" } } */
/* { dg-final { scan-assembler-not "movb\[ \\t\]+\\\$-1" } } */

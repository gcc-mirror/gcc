/* { dg-do compile } */
/* { dg-options "-O2 -Werror-implicit-function-declaration -march=k8 -msse2 -mmmx -mno-sse3 -mno-3dnow -mno-fma -mno-fxsr -mno-xsave -mno-rtm -mno-prfchw -mno-rdseed -mno-adx -mno-prefetchwt1 -mno-clflushopt -mno-xsavec -mno-xsaves -mno-clwb -mno-mwaitx -mno-clzero -mno-pku -mno-rdpid -mno-gfni -mno-shstk -mno-vaes -mno-vpclmulqdq" } */
/* { dg-add-options bind_pic_locally } */

/* We need to skip those intrin files which removed target attribute since after
   removal GCC will issue a "target option mismatch" error for those
   intrinsics. */

#define _AVX512VLDQINTRIN_H_INCLUDED

#include "sse-13.c"

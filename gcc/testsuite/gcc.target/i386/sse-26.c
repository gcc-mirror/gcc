/* { dg-do compile } */
/* { dg-options "-O2 -Werror-implicit-function-declaration -march=k8 -msse2 -mmmx -mno-sse3 -mno-3dnow -mno-fma -mno-fxsr -mno-xsave -mno-rtm -mno-prfchw -mno-rdseed -mno-adx -mno-prefetchwt1 -mno-clflushopt -mno-xsavec -mno-xsaves -mno-clwb -mno-mwaitx -mno-clzero -mno-pku -mno-rdpid -mno-gfni -mno-shstk -mno-vaes -mno-vpclmulqdq" } */
/* { dg-add-options bind_pic_locally } */

#include "sse-13.c"

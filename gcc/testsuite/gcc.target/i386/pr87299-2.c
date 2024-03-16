/* { dg-do compile } */
/* { dg-additional-options "-save-temps -mno-avx" } */

#pragma GCC target("avx")
const int x1 = __AVX__;

_Pragma("GCC target(\"avx512f\")")
const int x2 = __AVX512F__;

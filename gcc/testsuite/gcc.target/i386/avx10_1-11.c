/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -march=x86-64 -mavx10.1 -mno-avx512f" } */
/* { dg-warning "'-mno-avx512{f,vl,bw,dq,cd,bf16,fp16,vbmi,vbmi2,vnni,ifma,bitalg,vpopcntdq}' are ignored with '-mavx10.1' and above" "" { target *-*-* } 0 } */

#include "avx10_1-1.c"

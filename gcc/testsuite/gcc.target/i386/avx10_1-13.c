/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -march=x86-64 -mavx512f -mno-avx10.1" } */
/* { dg-warning "'-mno-avx10.1' is ignored when using with '-mavx512{f,vl,bw,dq,cd,bf16,fp16,vbmi,vbmi2,vnni,ifma,bitalg,vpopcntdq}'" "" { target *-*-* } 0 } */

#include "avx10_1-2.c"

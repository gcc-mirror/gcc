/* { dg-do compile { target ia32 } } */
/* { dg-additional-options "-O2 -march=x86-64 -m128bit-atomic" } */
/* { dg-error "'-m128bit-atomic' not supported for 32-bit code" "" { target *-*-* } 0 } */

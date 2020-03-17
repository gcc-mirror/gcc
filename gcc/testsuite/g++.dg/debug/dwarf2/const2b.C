/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O -gdwarf-2 -dA -msse" } */
/* { dg-require-effective-target sse } */
/* { dg-final { scan-assembler "DW_AT_const_value" } } */

typedef float FloatVect __attribute__((__vector_size__(16)));
const FloatVect Foo = { 250000000.0, 0.0, 0.0, 0.0 };

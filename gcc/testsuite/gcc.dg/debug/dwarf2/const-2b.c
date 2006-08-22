/* { dg-do compile { target i386*-*-* } } */
/* { dg-options "-O -gdwarf-2 -dA -msse" } */
/* { dg-final { scan-assembler "DW_AT_const_value" } } */

typedef float FloatVect __attribute__((__vector_size__(16)));
static FloatVect Foo = { 250000000.0, 0.0, 0.0, 0.0 };

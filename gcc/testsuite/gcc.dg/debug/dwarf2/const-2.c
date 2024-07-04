/* { dg-do compile } */
/* { dg-options "-O -gdwarf -dA -maltivec" } */
/* { dg-require-effective-target powerpc_altivec } */
/* { dg-final { scan-assembler "DW_AT_const_value" } } */

typedef float FloatVect __attribute__((__vector_size__(16)));
static FloatVect Foo = { 250000000.0, 0.0, 0.0, 0.0 };

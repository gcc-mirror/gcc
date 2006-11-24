/* { dg-do compile { target powerpc_altivec_ok } } */
/* { dg-options "-O -gdwarf-2 -dA -maltivec" } */
/* { dg-final { scan-assembler "DW_AT_const_value" } } */

typedef float FloatVect __attribute__((__vector_size__(16)));
static FloatVect Foo = { 250000000.0, 0.0, 0.0, 0.0 };

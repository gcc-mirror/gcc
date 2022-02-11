/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */

/* Test generating V2DImode constants that have the same bit pattern as
   V2DFmode constants that can be loaded with the XXSPLTIDP instruction with
   the ISA 3.1 (power10).  */

vector long long
vector_0 (void)
{
  /* XXSPLTIB or XXLXOR.  */
  return (vector long long) { 0LL, 0LL };
}

vector long long
vector_1 (void)
{
  /* XXSPLTIB and VEXTSB2D.  */
  return (vector long long) { 1LL, 1LL };
}

/* 0x8000000000000000LL is the bit pattern for -0.0, which can be generated
   with XXSPLTISDP.  */
vector long long
vector_float_neg_0 (void)
{
  /* XXSPLTIDP.  */
  return (vector long long) { 0x8000000000000000LL, 0x8000000000000000LL };
}

/* 0x3ff0000000000000LL is the bit pattern for 1.0 which can be generated with
   XXSPLTISDP.  */
vector long long
vector_float_1_0 (void)
{
  /* XXSPLTIDP.  */
  return (vector long long) { 0x3ff0000000000000LL, 0x3ff0000000000000LL };
}

/* 0x400921fb54442d18LL is the bit pattern for PI, which cannot be generated
   with XXSPLTIDP.  */
vector long long
scalar_pi (void)
{
  /* PLXV.  */
  return (vector long long) { 0x400921fb54442d18LL, 0x400921fb54442d18LL };
}

/* { dg-final { scan-assembler-times {\mxxspltidp\M} 2 } } */

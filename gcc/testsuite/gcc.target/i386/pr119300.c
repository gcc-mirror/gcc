/* { dg-do compile } */
/* { dg-options "-msoft-float -mfpmath=387 -msse" } */

float
foo (float f)
{
  return __builtin_ia32_rsqrtf (f);
}

/* { dg-warning "387 instruction set disabled, using SSE arithmetics" "" { target *-*-* } 0 } */

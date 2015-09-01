/* { dg-do compile } */
/* { dg-options "-O2 -mcpu=thunderx+nofp" } */

/* Make sure that we don't ICE when dealing with vector parameters
   in a simd-tagged function within a non-simd translation unit.  */

#pragma GCC push_options
#pragma GCC target ("+nothing+simd")
typedef unsigned int __uint32_t;
typedef __uint32_t uint32_t ;
typedef __Uint32x4_t uint32x4_t;
#pragma GCC pop_options


__attribute__ ((target ("cpu=cortex-a57")))
uint32x4_t
foo (uint32x4_t a, uint32_t b, uint32x4_t c)
{
  return c;
}

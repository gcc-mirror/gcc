/* { dg-do compile } */
/* { dg-options "-mavx512f" } */

void foo ()
{
  register int zmm_var asm ("zmm6") __attribute__((unused));

  __asm__ __volatile__("vpxord %%zmm0, %%zmm0, %%zmm7\n" : : : "zmm7" );
}

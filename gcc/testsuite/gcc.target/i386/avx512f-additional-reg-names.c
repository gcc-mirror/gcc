/* { dg-do compile } */
/* { dg-options "-mavx512f" } */

void foo ()
{
  register int zmm_var asm ("zmm9");

  __asm__ __volatile__("vxorpd %%zmm0, %%zmm0, %%zmm7\n" : : : "zmm7" );
}

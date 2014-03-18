/* { dg-do compile } */
/* { dg-options "-mavx" } */

void foo ()
{
  register int ymm_var asm ("ymm4");

  __asm__ __volatile__("vxorpd %%ymm0, %%ymm0, %%ymm7\n" : : : "ymm7" );
}

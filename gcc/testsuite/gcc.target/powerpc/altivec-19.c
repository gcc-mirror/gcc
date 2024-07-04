/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-options "-maltivec" } */
/* { dg-require-effective-target powerpc_altivec } */
/* { dg-final { scan-assembler "dst" } } */

void foo ( char* image )
{
  while ( 1 )
    {
      __builtin_altivec_dst( (void *)( (long)image & ~0x0f ), 0, 0 );
      image += 48;
    }
}

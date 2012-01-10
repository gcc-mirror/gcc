/* { dg-do compile } */
/* { dg-options "-ffast-math" } */
/* { dg-options "-ffast-math -msse" { target { { i?86-*-* x86_64-*-* } && ia32 } } } */
/* { dg-require-effective-target sse { target { { i?86-*-* x86_64-*-* } && ia32 } } } */
/* { dg-options "-ffast-math -mabi=altivec" { target { { powerpc*-*-linux* } && ia32 } } } */

#define __vector __attribute__((vector_size(16) ))
__vector float f(void);
void g(__vector float);
void RRB( __vector float vdist, __vector float vx)
{
  int detail;
  for(detail = 0; detail < 5;++detail)
  {
   __vector float frand = f();
   __vector float pullperc =  frand/ vdist;
   __vector float pullx =  vx * pullperc;
   g(pullx);
  }
}

/* Ignore a warning that is irrelevant to the purpose of this test.  */
/* { dg-prune-output ".*GCC vector returned by reference.*" } */

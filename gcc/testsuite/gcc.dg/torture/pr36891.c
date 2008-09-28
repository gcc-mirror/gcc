/* { dg-do compile } */
/* { dg-options "-ffast-math" } */

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

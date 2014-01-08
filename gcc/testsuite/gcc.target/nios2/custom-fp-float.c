/* Test generation of all single-float custom instructions.  */

/* { dg-do compile } */
/* { dg-options "-O1 -ffinite-math-only -funsafe-math-optimizations" } */

/* -O1 in the options is significant.  Without it FP operations may not be
   optimized to custom instructions.  */

#include <stdio.h> 
#include <math.h>

#pragma GCC target ("custom-fabss=100")
#pragma GCC target ("custom-fadds=101")
#pragma GCC target ("custom-fatans=102")
#pragma GCC target ("custom-fcoss=103")
#pragma GCC target ("custom-fdivs=104")
#pragma GCC target ("custom-fexps=105")
#pragma GCC target ("custom-flogs=106")
#pragma GCC target ("custom-fmaxs=107")
#pragma GCC target ("custom-fmins=108")
#pragma GCC target ("custom-fmuls=109")
#pragma GCC target ("custom-fnegs=110")
#pragma GCC target ("custom-fsins=111")
#pragma GCC target ("custom-fsqrts=112")
#pragma GCC target ("custom-fsubs=113")
#pragma GCC target ("custom-ftans=114")
#pragma GCC target ("custom-fcmpeqs=200")
#pragma GCC target ("custom-fcmpges=201")
#pragma GCC target ("custom-fcmpgts=202")
#pragma GCC target ("custom-fcmples=203")
#pragma GCC target ("custom-fcmplts=204")
#pragma GCC target ("custom-fcmpnes=205")

void
custom_fp (float a, float b, float *fp, int *ip)
{
  fp[0] = fabsf (a);
  fp[1] = a + b;
  fp[2] = atanf (a);
  fp[3] = cosf (a);
  fp[4] = a / b;
  fp[5] = expf (a);
  fp[6] = logf (a);
  fp[7] = fmaxf (a, b);
  fp[8] = fminf (a, b);
  fp[9] = a * b;
  fp[10] = -b;
  fp[11] = sinf (b);
  fp[12] = sqrtf (a);
  fp[13] = a - b;
  fp[14] = tanf (a);
  ip[0] = (a == fp[0]);
  ip[1] = (a >= fp[1]);
  ip[2] = (a > fp[2]);
  ip[3] = (a <= fp[3]);
  ip[4] = (a < fp[4]);
  ip[5] = (a != fp[5]);
}

/* { dg-final { scan-assembler "custom\\t100, .* # fabss .*" } } */
/* { dg-final { scan-assembler "custom\\t101, .* # fadds .*" } } */
/* { dg-final { scan-assembler "custom\\t102, .* # fatans .*" } } */
/* { dg-final { scan-assembler "custom\\t103, .* # fcoss .*" } } */
/* { dg-final { scan-assembler "custom\\t104, .* # fdivs .*" } } */
/* { dg-final { scan-assembler "custom\\t105, .* # fexps .*" } } */
/* { dg-final { scan-assembler "custom\\t106, .* # flogs .*" } } */
/* { dg-final { scan-assembler "custom\\t107, .* # fmaxs .*" } } */
/* { dg-final { scan-assembler "custom\\t108, .* # fmins .*" } } */
/* { dg-final { scan-assembler "custom\\t109, .* # fmuls .*" } } */
/* { dg-final { scan-assembler "custom\\t110, .* # fnegs .*" } } */
/* { dg-final { scan-assembler "custom\\t111, .* # fsins .*" } } */
/* { dg-final { scan-assembler "custom\\t112, .* # fsqrts .*" } } */
/* { dg-final { scan-assembler "custom\\t113, .* # fsubs .*" } } */
/* { dg-final { scan-assembler "custom\\t114, .* # ftans .*" } } */
/* { dg-final { scan-assembler "custom\\t200, .* # fcmpeqs .*" } } */
/* { dg-final { scan-assembler "custom\\t201, .* # fcmpges .*" } } */
/* { dg-final { scan-assembler "custom\\t202, .* # fcmpgts .*" } } */
/* { dg-final { scan-assembler "custom\\t203, .* # fcmples .*" } } */
/* { dg-final { scan-assembler "custom\\t204, .* # fcmplts .*" } } */
/* { dg-final { scan-assembler "custom\\t205, .* # fcmpnes .*" } } */

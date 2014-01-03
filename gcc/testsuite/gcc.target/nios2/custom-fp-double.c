/* Test generation of all double-float custom instructions.  */

/* { dg-do compile } */
/* { dg-options "-O1 -ffinite-math-only -funsafe-math-optimizations" } */

/* -O1 in the options is significant.  Without it FP operations may not be
   optimized to custom instructions.  */

#include <stdio.h> 
#include <math.h>

#pragma GCC target ("custom-frdxhi=40")
#pragma GCC target ("custom-frdxlo=41")
#pragma GCC target ("custom-frdy=42")
#pragma GCC target ("custom-fwrx=43")
#pragma GCC target ("custom-fwry=44")

#pragma GCC target ("custom-fabsd=100")
#pragma GCC target ("custom-faddd=101")
#pragma GCC target ("custom-fatand=102")
#pragma GCC target ("custom-fcosd=103")
#pragma GCC target ("custom-fdivd=104")
#pragma GCC target ("custom-fexpd=105")
#pragma GCC target ("custom-flogd=106")
#pragma GCC target ("custom-fmaxd=107")
#pragma GCC target ("custom-fmind=108")
#pragma GCC target ("custom-fmuld=109")
#pragma GCC target ("custom-fnegd=110")
#pragma GCC target ("custom-fsind=111")
#pragma GCC target ("custom-fsqrtd=112")
#pragma GCC target ("custom-fsubd=113")
#pragma GCC target ("custom-ftand=114")
#pragma GCC target ("custom-fcmpeqd=200")
#pragma GCC target ("custom-fcmpged=201")
#pragma GCC target ("custom-fcmpgtd=202")
#pragma GCC target ("custom-fcmpled=203")
#pragma GCC target ("custom-fcmpltd=204")
#pragma GCC target ("custom-fcmpned=205")

void
custom_fp (double a, double b, double *fp, int *ip)
{
  fp[0] = fabs (a);
  fp[1] = a + b;
  fp[2] = atan (a);
  fp[3] = cos (a);
  fp[4] = a / b;
  fp[5] = exp (a);
  fp[6] = log (a);
  fp[7] = fmax (a, b);
  fp[8] = fmin (a, b);
  fp[9] = a * b;
  fp[10] = -b;
  fp[11] = sin (b);
  fp[12] = sqrt (a);
  fp[13] = a - b;
  fp[14] = tan (a);
  ip[0] = (a == fp[0]);
  ip[1] = (a >= fp[1]);
  ip[2] = (a > fp[2]);
  ip[3] = (a <= fp[3]);
  ip[4] = (a < fp[4]);
  ip[5] = (a != fp[5]);
}

/* { dg-final { scan-assembler "custom\\t100, .* # fabsd .*" } } */
/* { dg-final { scan-assembler "custom\\t101, .* # faddd .*" } } */
/* { dg-final { scan-assembler "custom\\t102, .* # fatand .*" } } */
/* { dg-final { scan-assembler "custom\\t103, .* # fcosd .*" } } */
/* { dg-final { scan-assembler "custom\\t104, .* # fdivd .*" } } */
/* { dg-final { scan-assembler "custom\\t105, .* # fexpd .*" } } */
/* { dg-final { scan-assembler "custom\\t106, .* # flogd .*" } } */
/* { dg-final { scan-assembler "custom\\t107, .* # fmaxd .*" } } */
/* { dg-final { scan-assembler "custom\\t108, .* # fmind .*" } } */
/* { dg-final { scan-assembler "custom\\t109, .* # fmuld .*" } } */
/* { dg-final { scan-assembler "custom\\t110, .* # fnegd .*" } } */
/* { dg-final { scan-assembler "custom\\t111, .* # fsind .*" } } */
/* { dg-final { scan-assembler "custom\\t112, .* # fsqrtd .*" } } */
/* { dg-final { scan-assembler "custom\\t113, .* # fsubd .*" } } */
/* { dg-final { scan-assembler "custom\\t114, .* # ftand .*" } } */
/* { dg-final { scan-assembler "custom\\t200, .* # fcmpeqd .*" } } */
/* { dg-final { scan-assembler "custom\\t201, .* # fcmpged .*" } } */
/* { dg-final { scan-assembler "custom\\t202, .* # fcmpgtd .*" } } */
/* { dg-final { scan-assembler "custom\\t203, .* # fcmpled .*" } } */
/* { dg-final { scan-assembler "custom\\t204, .* # fcmpltd .*" } } */
/* { dg-final { scan-assembler "custom\\t205, .* # fcmpned .*" } } */

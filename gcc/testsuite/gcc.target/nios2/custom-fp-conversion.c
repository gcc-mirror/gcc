/* Test generation of conversion custom instructions.  */

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

#pragma GCC target ("custom-fextsd=100")
#pragma GCC target ("custom-fixdi=101")
#pragma GCC target ("custom-fixdu=102")
#pragma GCC target ("custom-fixsi=103")
#pragma GCC target ("custom-fixsu=104")
#pragma GCC target ("custom-floatid=105")
#pragma GCC target ("custom-floatis=106")
#pragma GCC target ("custom-floatud=107")
#pragma GCC target ("custom-floatus=108")
#pragma GCC target ("custom-ftruncds=109")

typedef struct data {
  double fextsd;
  int fixdi;
  unsigned fixdu;
  int fixsi;
  unsigned fixsu;
  double floatid;
  float floatis;
  double floatud;
  float floatus;
  float ftruncds;
} data_t;

void
custom_fp (int i, unsigned u, float f, double d, data_t *out)
{
  out->fextsd = (double) f;
  out->fixdi = (int) d;
  out->fixdu = (unsigned) d;
  out->fixsi = (int) f;
  out->fixsu = (unsigned) f;
  out->floatid = (double) i;
  out->floatis = (float) i;
  out->floatud = (double) u;
  out->floatus = (float) u;
  out->ftruncds = (float) d;
}

/* { dg-final { scan-assembler "custom\\t100, .* # fextsd .*" } } */
/* { dg-final { scan-assembler "custom\\t101, .* # fixdi .*" } } */
/* { dg-final { scan-assembler "custom\\t102, .* # fixdu .*" } } */
/* { dg-final { scan-assembler "custom\\t103, .* # fixsi .*" } } */
/* { dg-final { scan-assembler "custom\\t104, .* # fixsu .*" } } */
/* { dg-final { scan-assembler "custom\\t105, .* # floatid .*" } } */
/* { dg-final { scan-assembler "custom\\t106, .* # floatis .*" } } */
/* { dg-final { scan-assembler "custom\\t107, .* # floatud .*" } } */
/* { dg-final { scan-assembler "custom\\t108, .* # floatus .*" } } */
/* { dg-final { scan-assembler "custom\\t109, .* # ftruncds .*" } } */

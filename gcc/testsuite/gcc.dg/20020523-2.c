/* PR target/6753
   This testcase was miscompiled because sse_mov?fcc_const0*
   patterns were missing earlyclobber.  */
/* { dg-do run { target i386-*-* } } */
/* { dg-skip-if "" { i?86-*-* } { "-m64" } { "" } } */
/* { dg-options "-march=pentium3 -msse -ffast-math -O2" } */

#include "i386-cpuid.h"
extern void abort (void);
extern void exit (int);

float one = 1.f;

void bar (float f)
{
  if (__builtin_memcmp (&one, &f, sizeof (float)))
    abort ();
}

float foo (void)
{
  return 1.f;
}

typedef struct
{
  float t;
} T;

void bail_if_no_sse (void)
{
  unsigned int edx;
  /* See if capabilities include SSE (25th bit; 26 for SSE2).  */
  edx = i386_cpuid();
  if (!(edx & bit_SSE))
    exit (0);
}

int main (void)
{
  int i;
  T x[1];

  bail_if_no_sse ();
  for (i = 0; i < 1; i++)
    {
      x[i].t = foo ();
      x[i].t = 0.f > x[i].t ? 0.f : x[i].t;
      bar (x[i].t);
    }

  exit (0);
}

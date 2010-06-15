/* { dg-do compile { target x86_64-*-* } } */
/* { dg-options "-O2 -fdump-tree-esra-details" } */

#include "xmmintrin.h"

typedef __m128 v4sf; // vector of 4 floats (SSE1)

#define ARRSZ 1024

typedef union {
  float f[4];
  v4sf  v;
} V4SF;

struct COLOUR {
  float r,g,b;
};

void func (float *pre1, float pre2, struct COLOUR *a, V4SF *lpic)
  {
  V4SF va;
  int y;
  va.f[0]=a->r;va.f[1]=a->g;va.f[2]=a->b;va.f[3]=0.f;
  for (y=0; y<20; ++y)
    {
    float att = pre1[y]*pre2;
    v4sf tmpatt=_mm_load1_ps(&att);
    tmpatt=_mm_mul_ps(tmpatt,va.v);
    lpic[y].v=_mm_add_ps(tmpatt,lpic[y].v);
    }
  }

int main()
  {
  V4SF lpic[ARRSZ];
  float pre1[ARRSZ];
  int i;
  struct COLOUR col={0.,2.,4.};
  for (i=0; i<20; ++i)
    pre1[i]=0.4;
  for (i=0;i<10000000;++i)
    func(&pre1[0],0.3,&col,&lpic[0]);
  return 0;
  }

/* { dg-final { scan-tree-dump-times "Created a replacement" 0 "esra"} } */
/* { dg-final { cleanup-tree-dump "esra" } } */

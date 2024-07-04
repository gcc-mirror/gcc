/* { dg-do run } */
/* { dg-require-effective-target sse2 } */
/* { dg-require-effective-target c99_runtime } */
/* { dg-options "-O2 -msse2 -std=c99" } */

#include "sse2-check.h"
#include "emmintrin.h"
#include <math.h>

double sd1[2]={2134.3343,6678.346};
double sd2[2]={41124.234,6678.346};

float ss1[4]={2134.3343,6678.346,453.345635,54646.464356};
float ss2[4]={41124.234,6678.346,8653.65635,856.43576};

union
{
  double x[2];
  long long a[2];
}dd, ed;

union
{
  float x[4];
  int   a[4];
}ds, es;

#define CHECK(INTSIZE, SIZE, NUNITS, SUFFIX)			\
void check##SUFFIX(unsigned imm, char *id)		\
{							\
    if(checkV##INTSIZE(d##SIZE.a, e##SIZE.a, NUNITS)){	\
	printf("mm_cmp_" #SUFFIX "(%s: 0x%x) FAILED\n", id, imm);\
	abort();					\
    }							\
}

CHECK (l, d, 2, pd)
CHECK (i, s, 4, ps)
CHECK (l, d, 2, sd)
CHECK (i, s, 4, ss)

#define CMP(imm, rel, SIZE, NUNITS, SUFFIX)			\
    for (i = 0; i < NUNITS; i++) e##SIZE.a[i] = rel ? -1 : 0;	\
    source##SIZE##1 = _mm_loadu_p##SIZE(s##SIZE##1);			\
    source##SIZE##2 = _mm_loadu_p##SIZE(s##SIZE##2);			\
    dest##SIZE = _mm_cmp_##SUFFIX(source##SIZE##1, source##SIZE##2, imm);		\
    _mm_storeu_p##SIZE(d##SIZE.x, dest##SIZE);			\
    check##SUFFIX(imm, "" #imm "");

static void
sse2_test ()
{
    __m128d sourced1, sourced2, destd;
    __m128 sources1, sources2, dests;
    int i;

    CMP(_CMP_EQ_OQ, !isunordered(sd1[i], sd2[i]) && sd1[i] == sd2[i], d, 2, pd);
    CMP(_CMP_LT_OS, !isunordered(sd1[i], sd2[i]) && sd1[i] < sd2[i], d, 2, pd);
    CMP(_CMP_LE_OS, !isunordered(sd1[i], sd2[i]) && sd1[i] <= sd2[i], d, 2, pd);
    CMP(_CMP_UNORD_Q, isunordered(sd1[i], sd2[i]), d, 2, pd);
    CMP(_CMP_NEQ_UQ, isunordered(sd1[i], sd2[i]) || sd1[i] != sd2[i], d, 2, pd);
    CMP(_CMP_NLT_US, isunordered(sd1[i], sd2[i]) || sd1[i] >= sd2[i], d, 2, pd);
    CMP(_CMP_NLE_US, isunordered(sd1[i], sd2[i]) || sd1[i] > sd2[i], d, 2, pd);
    CMP(_CMP_ORD_Q, !isunordered(sd1[i], sd2[i]), d, 2, pd);

    CMP(_CMP_EQ_OQ, !isunordered(ss1[i], ss2[i]) && ss1[i] == ss2[i], s, 4, ps);
    CMP(_CMP_LT_OS, !isunordered(ss1[i], ss2[i]) && ss1[i] < ss2[i], s, 4, ps);
    CMP(_CMP_LE_OS, !isunordered(ss1[i], ss2[i]) && ss1[i] <= ss2[i], s, 4, ps);
    CMP(_CMP_UNORD_Q, isunordered(ss1[i], ss2[i]), s, 4, ps);
    CMP(_CMP_NEQ_UQ, isunordered(ss1[i], ss2[i]) || ss1[i] != ss2[i], s, 4, ps);
    CMP(_CMP_NLT_US, isunordered(ss1[i], ss2[i]) || ss1[i] >= ss2[i], s, 4, ps);
    CMP(_CMP_NLE_US, isunordered(ss1[i], ss2[i]) || ss1[i] > ss2[i], s, 4, ps);
    CMP(_CMP_ORD_Q, !isunordered(ss1[i], ss2[i]), s, 4, ps);

    dd.x[1] = ed.x[1] = sd1[1];
    
    CMP(_CMP_EQ_OQ, !isunordered(sd1[0], sd2[0]) && sd1[0] == sd2[0], d, 1, sd);
    CMP(_CMP_LT_OS, !isunordered(sd1[0], sd2[0]) && sd1[0] < sd2[0], d, 1, sd);
    CMP(_CMP_LE_OS, !isunordered(sd1[0], sd2[0]) && sd1[0] <= sd2[0], d, 1, sd);
    CMP(_CMP_UNORD_Q, isunordered(sd1[0], sd2[0]), d, 1, sd);
    CMP(_CMP_NEQ_UQ, isunordered(sd1[0], sd2[0]) || sd1[0] != sd2[0], d, 1, sd);
    CMP(_CMP_NLT_US, isunordered(sd1[0], sd2[0]) || sd1[0] >= sd2[0], d, 1, sd);
    CMP(_CMP_NLE_US, isunordered(sd1[0], sd2[0]) || sd1[0] > sd2[0], d, 1, sd);
    CMP(_CMP_ORD_Q, !isunordered(sd1[0], sd2[0]), d, 1, sd);

    for(i = 1; i < 4; i++) ds.x[i] = es.x[i] = ss1[i];
    
    CMP(_CMP_EQ_OQ, !isunordered(ss1[0], ss2[0]) && ss1[0] == ss2[0], s, 1, ss);
    CMP(_CMP_LT_OS, !isunordered(ss1[0], ss2[0]) && ss1[0] < ss2[0], s, 1, ss);
    CMP(_CMP_LE_OS, !isunordered(ss1[0], ss2[0]) && ss1[0] <= ss2[0], s, 1, ss);
    CMP(_CMP_UNORD_Q, isunordered(ss1[0], ss2[0]), s, 1, ss);
    CMP(_CMP_NEQ_UQ, isunordered(ss1[0], ss2[0]) || ss1[0] != ss2[0], s, 1, ss);
    CMP(_CMP_NLT_US, isunordered(ss1[0], ss2[0]) || ss1[0] >= ss2[0], s, 1, ss);
    CMP(_CMP_NLE_US, isunordered(ss1[0], ss2[0]) || ss1[0] > ss2[0], s, 1, ss);
    CMP(_CMP_ORD_Q, !isunordered(ss1[0], ss2[0]), s, 1, ss);
}

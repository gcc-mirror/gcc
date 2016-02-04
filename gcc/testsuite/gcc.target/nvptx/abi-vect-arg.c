/* { dg-do compile } */
/* { dg-additional-options "-Wno-pedantic -Wno-long-long -m64" } */

/* Vector arg.  Pass via pointer.  */

typedef char __attribute__ ((vector_size (1))) vc1;
typedef char __attribute__ ((vector_size (2))) vc2;
typedef char __attribute__ ((vector_size (4))) vc4;
typedef char __attribute__ ((vector_size (8))) vc8;

typedef short __attribute__ ((vector_size (2))) vs1;
typedef short __attribute__ ((vector_size (4))) vs2;
typedef short __attribute__ ((vector_size (8))) vs4;
typedef short __attribute__ ((vector_size (16))) vs8;

typedef int __attribute__ ((vector_size (4))) vi1;
typedef int __attribute__ ((vector_size (8))) vi2;
typedef int __attribute__ ((vector_size (16))) vi4;
typedef int __attribute__ ((vector_size (32))) vi8;

typedef long long __attribute__ ((vector_size (8))) vll1;
typedef long long __attribute__ ((vector_size (16))) vll2;
typedef long long __attribute__ ((vector_size (32))) vll4;
typedef long long __attribute__ ((vector_size (64))) vll8;

typedef float __attribute__ ((vector_size (4))) vf1;
typedef float __attribute__ ((vector_size (8))) vf2;
typedef float __attribute__ ((vector_size (16))) vf4;
typedef float __attribute__ ((vector_size (32))) vf8;

typedef double __attribute__ ((vector_size (8))) vd1;
typedef double __attribute__ ((vector_size (16))) vd2;
typedef double __attribute__ ((vector_size (32))) vd4;
typedef double __attribute__ ((vector_size (64))) vd8;

/* { dg-final { scan-assembler-times ".extern .func dcl_avc1 \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
void dcl_avc1 (vc1);
/* { dg-final { scan-assembler-times ".extern .func dcl_avc2 \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
void dcl_avc2 (vc2);
/* { dg-final { scan-assembler-times ".extern .func dcl_avc4 \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
void dcl_avc4 (vc4);
/* { dg-final { scan-assembler-times ".extern .func dcl_avc8 \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
void dcl_avc8 (vc8);

/* { dg-final { scan-assembler-times ".extern .func dcl_avs1 \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
void dcl_avs1 (vs1);
/* { dg-final { scan-assembler-times ".extern .func dcl_avs2 \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
void dcl_avs2 (vs2);
/* { dg-final { scan-assembler-times ".extern .func dcl_avs4 \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
void dcl_avs4 (vs4);
/* { dg-final { scan-assembler-times ".extern .func dcl_avs8 \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
void dcl_avs8 (vs8);

/* { dg-final { scan-assembler-times ".extern .func dcl_avi1 \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
void dcl_avi1 (vi1);
/* { dg-final { scan-assembler-times ".extern .func dcl_avi2 \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
void dcl_avi2 (vi2);
/* { dg-final { scan-assembler-times ".extern .func dcl_avi4 \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
void dcl_avi4 (vi4);
/* { dg-final { scan-assembler-times ".extern .func dcl_avi8 \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
void dcl_avi8 (vi8);

/* { dg-final { scan-assembler-times ".extern .func dcl_avll1 \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
void dcl_avll1 (vll1);
/* { dg-final { scan-assembler-times ".extern .func dcl_avll2 \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
void dcl_avll2 (vll2);
/* { dg-final { scan-assembler-times ".extern .func dcl_avll4 \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
void dcl_avll4 (vll4);
/* { dg-final { scan-assembler-times ".extern .func dcl_avll8 \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
void dcl_avll8 (vll8);

/* { dg-final { scan-assembler-times ".extern .func dcl_avf1 \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
void dcl_avf1 (vf1);
/* { dg-final { scan-assembler-times ".extern .func dcl_avf2 \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
void dcl_avf2 (vf2);
/* { dg-final { scan-assembler-times ".extern .func dcl_avf4 \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
void dcl_avf4 (vf4);
/* { dg-final { scan-assembler-times ".extern .func dcl_avf8 \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
void dcl_avf8 (vf8);

/* { dg-final { scan-assembler-times ".extern .func dcl_avd1 \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
void dcl_avd1 (vd1);
/* { dg-final { scan-assembler-times ".extern .func dcl_avd2 \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
void dcl_avd2 (vd2);
/* { dg-final { scan-assembler-times ".extern .func dcl_avd4 \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
void dcl_avd4 (vd4);
/* { dg-final { scan-assembler-times ".extern .func dcl_avd8 \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
void dcl_avd8 (vd8);

#define M(T, V) ({T t;t[0]= V;t;})

void  test_1 (void)
{
  dcl_avc1 (M (vc1, 1));
  dcl_avc2 (M (vc2, 2));
  dcl_avc4 (M (vc4, 3));
  dcl_avc8 (M (vc8, 4));

  dcl_avs1 (M (vs1, 5));
  dcl_avs2 (M (vs2, 6));
  dcl_avs4 (M (vs4, 7));
  dcl_avs8 (M (vs8, 8));

  dcl_avi1 (M (vi1, 9));
  dcl_avi2 (M (vi2, 10));
  dcl_avi4 (M (vi4, 11));
  dcl_avi8 (M (vi8, 12));

  dcl_avll1 (M (vll1, 13));
  dcl_avll2 (M (vll2, 14));
  dcl_avll4 (M (vll4, 15));
  dcl_avll8 (M (vll8, 16));

  dcl_avf1 (M (vf1, 17));
  dcl_avf2 (M (vf2, 18));
  dcl_avf4 (M (vf4, 19));
  dcl_avf8 (M (vf8, 20));

  dcl_avd1 (M (vd1, 21));
  dcl_avd2 (M (vd2, 22));
  dcl_avd4 (M (vd4, 23));
  dcl_avd8 (M (vd8, 24));
}

/* { dg-final { scan-assembler-times ".visible .func dfn_avc1 \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
void dfn_avc1(vc1 a)
{
}
/* { dg-final { scan-assembler-times ".visible .func dfn_avc2 \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
void dfn_avc2(vc2 a)
{
}
/* { dg-final { scan-assembler-times ".visible .func dfn_avc4 \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
void dfn_avc4(vc4 a)
{
}
/* { dg-final { scan-assembler-times ".visible .func dfn_avc8 \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
void dfn_avc8(vc8 a)
{
}

/* { dg-final { scan-assembler-times ".visible .func dfn_avs1 \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
void dfn_avs1(vs1 a)
{
}
/* { dg-final { scan-assembler-times ".visible .func dfn_avs2 \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
void dfn_avs2(vs2 a)
{
}
/* { dg-final { scan-assembler-times ".visible .func dfn_avs4 \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
void dfn_avs4(vs4 a)
{
}
/* { dg-final { scan-assembler-times ".visible .func dfn_avs8 \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
void dfn_avs8(vs8 a)
{
}

/* { dg-final { scan-assembler-times ".visible .func dfn_avi1 \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
void dfn_avi1(vi1 a)
{
}
/* { dg-final { scan-assembler-times ".visible .func dfn_avi2 \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
void dfn_avi2(vi2 a)
{
}
/* { dg-final { scan-assembler-times ".visible .func dfn_avi4 \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
void dfn_avi4(vi4 a)
{
}
/* { dg-final { scan-assembler-times ".visible .func dfn_avi8 \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
void dfn_avi8(vi8 a)
{
}

/* { dg-final { scan-assembler-times ".visible .func dfn_avll1 \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
void dfn_avll1(vll1 a)
{
}
/* { dg-final { scan-assembler-times ".visible .func dfn_avll2 \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
void dfn_avll2(vll2 a)
{
}
/* { dg-final { scan-assembler-times ".visible .func dfn_avll4 \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
void dfn_avll4(vll4 a)
{
}
/* { dg-final { scan-assembler-times ".visible .func dfn_avll8 \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
void dfn_avll8(vll8 a)
{
}

/* { dg-final { scan-assembler-times ".visible .func dfn_avf1 \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
void dfn_avf1(vf1 a)
{
}
/* { dg-final { scan-assembler-times ".visible .func dfn_avf2 \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
void dfn_avf2(vf2 a)
{
}
/* { dg-final { scan-assembler-times ".visible .func dfn_avf4 \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
void dfn_avf4(vf4 a)
{
}
/* { dg-final { scan-assembler-times ".visible .func dfn_avf8 \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
void dfn_avf8(vf8 a)
{
}

/* { dg-final { scan-assembler-times ".visible .func dfn_avd1 \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
void dfn_avd1(vd1 a)
{
}
/* { dg-final { scan-assembler-times ".visible .func dfn_avd2 \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
void dfn_avd2(vd2 a)
{
}
/* { dg-final { scan-assembler-times ".visible .func dfn_avd4 \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
void dfn_avd4(vd4 a)
{
}
/* { dg-final { scan-assembler-times ".visible .func dfn_avd8 \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
void dfn_avd8(vd8 a)
{
}

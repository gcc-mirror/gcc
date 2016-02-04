/* { dg-do compile } */
/* { dg-additional-options "-Wno-pedantic -Wno-long-long -m64" } */

/* Vector return.  Return via pointer.  */

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

/* { dg-final { scan-assembler-times ".extern .func dcl_rvc1 \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
vc1 dcl_rvc1 (void);
/* { dg-final { scan-assembler-times ".extern .func dcl_rvc2 \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
vc2 dcl_rvc2 (void);
/* { dg-final { scan-assembler-times ".extern .func dcl_rvc4 \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
vc4 dcl_rvc4 (void);
/* { dg-final { scan-assembler-times ".extern .func dcl_rvc8 \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
vc8 dcl_rvc8 (void);

/* { dg-final { scan-assembler-times ".extern .func dcl_rvs1 \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
vs1 dcl_rvs1 (void);
/* { dg-final { scan-assembler-times ".extern .func dcl_rvs2 \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
vs2 dcl_rvs2 (void);
/* { dg-final { scan-assembler-times ".extern .func dcl_rvs4 \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
vs4 dcl_rvs4 (void);
/* { dg-final { scan-assembler-times ".extern .func dcl_rvs8 \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
vs8 dcl_rvs8 (void);

/* { dg-final { scan-assembler-times ".extern .func dcl_rvi1 \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
vi1 dcl_rvi1 (void);
/* { dg-final { scan-assembler-times ".extern .func dcl_rvi2 \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
vi2 dcl_rvi2 (void);
/* { dg-final { scan-assembler-times ".extern .func dcl_rvi4 \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
vi4 dcl_rvi4 (void);
/* { dg-final { scan-assembler-times ".extern .func dcl_rvi8 \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
vi8 dcl_rvi8 (void);

/* { dg-final { scan-assembler-times ".extern .func dcl_rvll1 \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
vll1 dcl_rvll1 (void);
/* { dg-final { scan-assembler-times ".extern .func dcl_rvll2 \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
vll2 dcl_rvll2 (void);
/* { dg-final { scan-assembler-times ".extern .func dcl_rvll4 \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
vll4 dcl_rvll4 (void);
/* { dg-final { scan-assembler-times ".extern .func dcl_rvll8 \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
vll8 dcl_rvll8 (void);

/* { dg-final { scan-assembler-times ".extern .func dcl_rvf1 \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
vf1 dcl_rvf1 (void);
/* { dg-final { scan-assembler-times ".extern .func dcl_rvf2 \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
vf2 dcl_rvf2 (void);
/* { dg-final { scan-assembler-times ".extern .func dcl_rvf4 \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
vf4 dcl_rvf4 (void);
/* { dg-final { scan-assembler-times ".extern .func dcl_rvf8 \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
vf8 dcl_rvf8 (void);

/* { dg-final { scan-assembler-times ".extern .func dcl_rvd1 \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
vd1 dcl_rvd1 (void);
/* { dg-final { scan-assembler-times ".extern .func dcl_rvd2 \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
vd2 dcl_rvd2 (void);
/* { dg-final { scan-assembler-times ".extern .func dcl_rvd4 \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
vd4 dcl_rvd4 (void);
/* { dg-final { scan-assembler-times ".extern .func dcl_rvd8 \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
vd8 dcl_rvd8 (void);

void  test_1 (void)
{
  dcl_rvc1 ();
  dcl_rvc2 ();
  dcl_rvc4 ();
  dcl_rvc8 ();
  
  dcl_rvs1 ();
  dcl_rvs2 ();
  dcl_rvs4 ();
  dcl_rvs8 ();
  
  dcl_rvi1 ();
  dcl_rvi2 ();
  dcl_rvi4 ();
  dcl_rvi8 ();

  dcl_rvll1 ();
  dcl_rvll2 ();
  dcl_rvll4 ();
  dcl_rvll8 ();
  
  dcl_rvf1 ();
  dcl_rvf2 ();
  dcl_rvf4 ();
  dcl_rvf8 ();

  dcl_rvd1 ();
  dcl_rvd2 ();
  dcl_rvd4 ();
  dcl_rvd8 ();
}

#define M(T, V) ({T t;t[0]= V;t;})

/* { dg-final { scan-assembler-times ".visible .func dfn_rvc1 \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
vc1 dfn_rvc1 (void)
{
  return M (vc1, 1);
}
/* { dg-final { scan-assembler-times ".visible .func dfn_rvc2 \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
vc2 dfn_rvc2 (void)
{
  return M (vc2, 2);
}
/* { dg-final { scan-assembler-times ".visible .func dfn_rvc4 \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
vc4 dfn_rvc4 (void)
{
  return M (vc4, 3);
}
/* { dg-final { scan-assembler-times ".visible .func dfn_rvc8 \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
vc8 dfn_rvc8 (void)
{
  return M (vc8, 4);
}

/* { dg-final { scan-assembler-times ".visible .func dfn_rvs1 \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
vs1 dfn_rvs1 (void)
{
  return M (vs1, 5);
}
/* { dg-final { scan-assembler-times ".visible .func dfn_rvs2 \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
vs2 dfn_rvs2 (void)
{
  return M (vs2, 6);
}
/* { dg-final { scan-assembler-times ".visible .func dfn_rvs4 \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
vs4 dfn_rvs4 (void)
{
  return M (vs4, 7);
}
/* { dg-final { scan-assembler-times ".visible .func dfn_rvs8 \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
vs8 dfn_rvs8 (void)
{
  return M (vs8, 8);
}

/* { dg-final { scan-assembler-times ".visible .func dfn_rvi1 \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
vi1 dfn_rvi1 (void)
{
  return M (vi1, 9);
}
/* { dg-final { scan-assembler-times ".visible .func dfn_rvi2 \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
vi2 dfn_rvi2 (void)
{
  return M (vi2, 10);
}
/* { dg-final { scan-assembler-times ".visible .func dfn_rvi4 \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
vi4 dfn_rvi4 (void)
{
  return M (vi4, 11);
}
/* { dg-final { scan-assembler-times ".visible .func dfn_rvi8 \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
vi8 dfn_rvi8 (void)
{
  return M (vi8, 12);
}

/* { dg-final { scan-assembler-times ".visible .func dfn_rvll1 \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
vll1 dfn_rvll1 (void)
{
  return M (vll1, 13);
}
/* { dg-final { scan-assembler-times ".visible .func dfn_rvll2 \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
vll2 dfn_rvll2 (void)
{
  return M (vll2, 14);
}
/* { dg-final { scan-assembler-times ".visible .func dfn_rvll4 \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
vll4 dfn_rvll4 (void)
{
  return M (vll4, 16);
}
/* { dg-final { scan-assembler-times ".visible .func dfn_rvll8 \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
vll8 dfn_rvll8 (void)
{
  return M (vll8, 6);
}

/* { dg-final { scan-assembler-times ".visible .func dfn_rvf1 \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
vf1 dfn_rvf1 (void)
{
  return M (vf1, 17);
}
/* { dg-final { scan-assembler-times ".visible .func dfn_rvf2 \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
vf2 dfn_rvf2 (void)
{
  return M (vf2, 18);
}
/* { dg-final { scan-assembler-times ".visible .func dfn_rvf4 \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
vf4 dfn_rvf4 (void)
{
  return M (vf4, 19);
}
/* { dg-final { scan-assembler-times ".visible .func dfn_rvf8 \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
vf8 dfn_rvf8 (void)
{
  return M (vf8, 20);
}

/* { dg-final { scan-assembler-times ".visible .func dfn_rvd1 \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
vd1 dfn_rvd1 (void)
{
  return M (vd1, 21);
}
/* { dg-final { scan-assembler-times ".visible .func dfn_rvd2 \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
vd2 dfn_rvd2 (void)
{
  return M (vd2, 22);
}
/* { dg-final { scan-assembler-times ".visible .func dfn_rvd4 \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
vd4 dfn_rvd4 (void)
{
  return M (vd4, 23);
}
/* { dg-final { scan-assembler-times ".visible .func dfn_rvd8 \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
vd8 dfn_rvd8 (void)
{
  return M (vd8, 24);
}

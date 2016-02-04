/* { dg-do compile } */
/* { dg-additional-options "-Wno-pedantic -Wno-long-long -m64" } */

/* Complex return.  Returned via pointer.  */

/* { dg-final { scan-assembler-times ".extern .func dcl_rcc \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
_Complex char dcl_rcc (void);

/* { dg-final { scan-assembler-times ".extern .func dcl_rcs \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
_Complex short dcl_rcs (void);

/* { dg-final { scan-assembler-times ".extern .func dcl_rci \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
_Complex int dcl_rci (void);

/* { dg-final { scan-assembler-times ".extern .func dcl_rcll \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
_Complex long long dcl_rcll (void);

/* { dg-final { scan-assembler-times ".extern .func dcl_rcf \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
_Complex float dcl_rcf (void);

/* { dg-final { scan-assembler-times ".extern .func dcl_rcd \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
_Complex double dcl_rcd (void);

void test_1 (void)
{
  dcl_rcc ();
  dcl_rcs ();
  dcl_rci ();
  dcl_rcll ();
  dcl_rcf ();
  dcl_rcd ();
}

#define M(T,r,i) ({_Complex T x; __real__ (x) = (r), __imag__(x) == (i); x; })

/* { dg-final { scan-assembler-times ".visible .func dfn_rcc \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
_Complex char dfn_rcc (void)
{
  return M (char,1,2);
}

/* { dg-final { scan-assembler-times ".visible .func dfn_rcs \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
_Complex short dfn_rcs (void)
{
  return M (short,3,4);
}

/* { dg-final { scan-assembler-times ".visible .func dfn_rci \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
_Complex int dfn_rci (void)
{
  return M (int,5,6);
}

/* { dg-final { scan-assembler-times ".visible .func dfn_rcll \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
_Complex long long dfn_rcll (void)
{
  return M (long long,7,8);
}

/* { dg-final { scan-assembler-times ".visible .func dfn_rcf \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
_Complex float dfn_rcf (void)
{
  return M (float,9,10);
}

/* { dg-final { scan-assembler-times ".visible .func dfn_rcd \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
_Complex double dfn_rcd (void)
{
  return M (double,11,12);
}

/* { dg-do compile } */
/* { dg-additional-options "-Wno-pedantic -Wno-long-long -m64" } */

/* Comples arg types.  All these should be in 2 registers.  */

/* { dg-final { scan-assembler-times ".extern .func dcl_acc \\(.param.u32 %\[_a-z0-9\]*, .param.u32 %\[_a-z0-9\]*\\);" 1 } } */
void dcl_acc (_Complex char);

/* { dg-final { scan-assembler-times ".extern .func dcl_acs \\(.param.u32 %\[_a-z0-9\]*, .param.u32 %\[_a-z0-9\]*\\);" 1 } } */
void dcl_acs (_Complex short);

/* { dg-final { scan-assembler-times ".extern .func dcl_aci \\(.param.u32 %\[_a-z0-9\]*, .param.u32 %\[_a-z0-9\]*\\);" 1 } } */
void dcl_aci (_Complex int);

/* { dg-final { scan-assembler-times ".extern .func dcl_acll \\(.param.u64 %\[_a-z0-9\]*, .param.u64 %\[_a-z0-9\]*\\);" 1 } } */
void dcl_acll (_Complex long);

/* { dg-final { scan-assembler-times ".extern .func dcl_acf \\(.param.f32 %\[_a-z0-9\]*, .param.f32 %\[_a-z0-9\]*\\);" 1 } } */
void dcl_acf (_Complex float);

/* { dg-final { scan-assembler-times ".extern .func dcl_acd \\(.param.f64 %\[_a-z0-9\]*, .param.f64 %\[_a-z0-9\]*\\);" 1 } } */
void dcl_acd (_Complex double);

#define M(T,r,i) ({_Complex T x; __real__ (x) = (r), __imag__(x) == (i); x; })

void test_1 (void)
{
  dcl_acc (M (char, 1, 2));
  dcl_acs (M (short, 3, 4));
  dcl_aci (M (int, 5, 6));
  dcl_acll (M (long long, 7, 8));
  dcl_acf (M (float, 9, 10));
  dcl_acd (M (double, 11, 12));
}

/* { dg-final { scan-assembler-times ".visible .func dfn_acc \\(.param.u32 %\[_a-z0-9\]*, .param.u32 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
void dfn_acc (_Complex char c)
{
}

/* { dg-final { scan-assembler-times ".visible .func dfn_acs \\(.param.u32 %\[_a-z0-9\]*, .param.u32 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
void dfn_acs (_Complex short s)
{
}

/* { dg-final { scan-assembler-times ".visible .func dfn_aci \\(.param.u32 %\[_a-z0-9\]*, .param.u32 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
void dfn_aci (_Complex int i)
{
}

/* { dg-final { scan-assembler-times ".visible .func dfn_acll \\(.param.u64 %\[_a-z0-9\]*, .param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
void dfn_acll (_Complex long long ll)
{
}

/* { dg-final { scan-assembler-times ".visible .func dfn_acf \\(.param.f32 %\[_a-z0-9\]*, .param.f32 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
void dfn_acf (_Complex float f)
{
}

/* { dg-final { scan-assembler-times ".visible .func dfn_acd \\(.param.f64 %\[_a-z0-9\]*, .param.f64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
void dfn_acd (_Complex double d)
{
}

/* { dg-do compile } */
/* { dg-additional-options "-Wno-pedantic -Wno-long-long -m64" } */

/* Scalar return types.  In registers when <= 64 bit.  */

typedef int __attribute__((mode(TI))) ti;

/* { dg-final { scan-assembler-times ".extern .func dcl_av;" 1 } } */
void dcl_av (void);

/* { dg-final { scan-assembler-times ".extern .func dcl_ac \\(.param.u32 %\[_a-z0-9\]*\\);" 1 } } */
void dcl_ac (char);

/* { dg-final { scan-assembler-times ".extern .func dcl_as \\(.param.u32 %\[_a-z0-9\]*\\);" 1 } } */
void dcl_as (short);

/* { dg-final { scan-assembler-times ".extern .func dcl_ai \\(.param.u32 %\[_a-z0-9\]*\\);" 1 } } */
void dcl_ai (int);

/* { dg-final { scan-assembler-times ".extern .func dcl_all \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
void dcl_all (long long);

/* { dg-final { scan-assembler-times ".extern .func dcl_ati \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
void dcl_ati (ti);

/* { dg-final { scan-assembler-times ".extern .func dcl_af \\(.param.f32 %\[_a-z0-9\]*\\);" 1 } } */
void dcl_af (float);

/* { dg-final { scan-assembler-times ".extern .func dcl_ad \\(.param.f64 %\[_a-z0-9\]*\\);" 1 } } */
void dcl_ad (double);

/* { dg-final { scan-assembler-times ".extern .func dcl_ap \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
void dcl_ap (void *);

void test_1 (void)
{
  dcl_av ();
  dcl_ac (1);
  dcl_as (2);
  dcl_ai (3);
  dcl_all (4);
  dcl_ati (5);
  dcl_af (6);
  dcl_ad (7);
  dcl_ap (0);
}

/* { dg-final { scan-assembler-times ".visible .func dfn_av(?:;|\[\r\n\]+\{)" 2 } } */
void dfn_av (void)
{
}

/* { dg-final { scan-assembler-times ".visible .func dfn_ac \\(.param.u32 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
void dfn_ac (char c)
{
}

/* { dg-final { scan-assembler-times ".visible .func dfn_as \\(.param.u32 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
void dfn_as (short s)
{
}

/* { dg-final { scan-assembler-times ".visible .func dfn_ai \\(.param.u32 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
void dfn_ai (int i)
{
}

/* { dg-final { scan-assembler-times ".visible .func dfn_all \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
void dfn_all (long long ll)
{
}

/* { dg-final { scan-assembler-times ".visible .func dfn_ati \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
void dfn_ati (ti t)
{
}

/* { dg-final { scan-assembler-times ".visible .func dfn_af \\(.param.f32 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
void dfn_af (float f)
{
}

/* { dg-final { scan-assembler-times ".visible .func dfn_ad \\(.param.f64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
void dfn_ad (double d)
{
}

/* { dg-final { scan-assembler-times ".visible .func dfn_ap \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
void dfn_ap (void *p)
{
}

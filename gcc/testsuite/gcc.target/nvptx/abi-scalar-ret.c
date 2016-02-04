/* { dg-do compile } */
/* { dg-additional-options "-Wno-pedantic -Wno-long-long -m64" } */

/* Scalar return types.  In registers when <= 64 bit.  */

typedef int __attribute__((mode(TI))) ti;

/* { dg-final { scan-assembler-times ".extern .func dcl_rv;" 1 } } */
void dcl_rv (void);

/* { dg-final { scan-assembler-times ".extern .func \\(.param.u32 %\[_a-z\]*\\) dcl_rc;" 1 } } */
char dcl_rc (void);

/* { dg-final { scan-assembler-times ".extern .func \\(.param.u32 %\[_a-z\]*\\) dcl_rs;" 1 } } */
short dcl_rs (void);

/* { dg-final { scan-assembler-times ".extern .func \\(.param.u32 %\[_a-z\]*\\) dcl_ri;" 1 } } */
int dcl_ri (void);

/* { dg-final { scan-assembler-times ".extern .func \\(.param.u64 %\[_a-z\]*\\) dcl_rll;" 1 } } */
long long dcl_rll (void);

/* { dg-final { scan-assembler-times ".extern .func dcl_rti \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
ti dcl_rti (void);

/* { dg-final { scan-assembler-times ".extern .func \\(.param.f32 %\[_a-z\]*\\) dcl_rf;" 1 } } */
float dcl_rf (void);

/* { dg-final { scan-assembler-times ".extern .func \\(.param.f64 %\[_a-z\]*\\) dcl_rd;" 1 } } */
double dcl_rd (void);

/* { dg-final { scan-assembler-times ".extern .func \\(.param.u64 %\[_a-z\]*\\) dcl_rp;" 1 } } */
void *dcl_rp (void);

void test_1 (void)
{
  dcl_rv ();
  dcl_rc ();
  dcl_rs ();
  dcl_ri ();
  dcl_rll ();
  dcl_rti ();
  dcl_rf ();
  dcl_rd ();
  dcl_rp ();
}

/* { dg-final { scan-assembler-times ".visible .func dfn_rv(?:;|\[\r\n\]+\{)" 2 } } */
void dfn_rv (void)
{
  return;
}

/* { dg-final { scan-assembler-times ".visible .func \\(.param.u32 %\[_a-z0-9\]*\\) dfn_rc(?:;|\[\r\n\]+\{)" 2 } } */
char dfn_rc (void)
{
  return 1;
}

/* { dg-final { scan-assembler-times ".visible .func \\(.param.u32 %\[_a-z0-0\]*\\) dfn_rs(?:;|\[\r\n\]+\{)" 2 } } */
short dfn_rs (void)
{
  return 2;
}

/* { dg-final { scan-assembler-times ".visible .func \\(.param.u32 %\[_a-z0-9\]*\\) dfn_ri(?:;|\[\r\n\]+\{)" 2 } } */
int dfn_ri (void)
{
  return 3;
}

/* { dg-final { scan-assembler-times ".visible .func \\(.param.u64 %\[_a-z0-9\]*\\) dfn_rll(?:;|\[\r\n\]+\{)" 2 } } */
long long dfn_rll (void)
{
  return 4;
}

/* { dg-final { scan-assembler-times ".visible .func dfn_rti \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
ti dfn_rti (void)
{
  return 5;
}

/* { dg-final { scan-assembler-times ".visible .func \\(.param.f32 %\[_a-z0-9\]*\\) dfn_rf(?:;|\[\r\n\]+\{)" 2 } } */
float dfn_rf (void)
{
  return 6;
}

/* { dg-final { scan-assembler-times ".visible .func \\(.param.f64 %\[_a-z0-9\]*\\) dfn_rd(?:;|\[\r\n\]+\{)" 2 } } */
double dfn_rd (void)
{
  return 7;
}

/* { dg-final { scan-assembler-times ".visible .func \\(.param.u64 %\[_a-z0-9\]*\\) dfn_rp(?:;|\[\r\n\]+\{)" 2 } } */
void *dfn_rp (void)
{
  return 0;
}

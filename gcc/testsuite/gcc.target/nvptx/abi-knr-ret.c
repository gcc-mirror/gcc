/* { dg-do compile } */
/* { dg-additional-options "-Wno-pedantic -Wno-long-long -m64" } */

/* K+R returns.  as for ANSI .  */

/* { dg-final { scan-assembler-times ".extern .func dcl_rv;" 1 } } */
void dcl_rv ();

/* { dg-final { scan-assembler-times ".extern .func \\(.param.u32 %\[_a-z\]*\\) dcl_rc;" 1 } } */
char dcl_rc ();

/* { dg-final { scan-assembler-times ".extern .func \\(.param.u32 %\[_a-z\]*\\) dcl_rs;" 1 } } */
short dcl_rs ();

/* { dg-final { scan-assembler-times ".extern .func \\(.param.u32 %\[_a-z\]*\\) dcl_ri;" 1 } } */
int dcl_ri ();

/* { dg-final { scan-assembler-times ".extern .func \\(.param.u64 %\[_a-z\]*\\) dcl_rll;" 1 } } */
long long dcl_rll ();

/* { dg-final { scan-assembler-times ".extern .func \\(.param.f32 %\[_a-z\]*\\) dcl_rf;" 1 } } */
float dcl_rf ();

/* { dg-final { scan-assembler-times ".extern .func \\(.param.f64 %\[_a-z\]*\\) dcl_rd;" 1 } } */
double dcl_rd ();

/* { dg-final { scan-assembler-times ".extern .func \\(.param.u64 %\[_a-z\]*\\) dcl_rp;" 1 } } */
void *dcl_rp ();

void test_1 ()
{
  dcl_rv ();
  dcl_rc ();
  dcl_rs ();
  dcl_ri ();
  dcl_rll ();
  dcl_rf ();
  dcl_rd ();
  dcl_rp ();
}

/* { dg-final { scan-assembler-times ".visible .func dfn_rv(?:;|\[\r\n\]+\{)" 2 } } */
void dfn_rv ()
{
  return;
}

/* { dg-final { scan-assembler-times ".visible .func \\(.param.u32 %\[_a-z0-9\]*\\) dfn_rc(?:;|\[\r\n\]+\{)" 2 } } */
char dfn_rc ()
{
  return 1;
}

/* { dg-final { scan-assembler-times ".visible .func \\(.param.u32 %\[_a-z0-0\]*\\) dfn_rs(?:;|\[\r\n\]+\{)" 2 } } */
short dfn_rs ()
{
  return 2;
}

/* { dg-final { scan-assembler-times ".visible .func \\(.param.u32 %\[_a-z0-9\]*\\) dfn_ri(?:;|\[\r\n\]+\{)" 2 } } */
int dfn_ri ()
{
  return 3;
}

/* { dg-final { scan-assembler-times ".visible .func \\(.param.u64 %\[_a-z0-9\]*\\) dfn_rll(?:;|\[\r\n\]+\{)" 2 } } */
long long dfn_rll ()
{
  return 4;
}

/* { dg-final { scan-assembler-times ".visible .func \\(.param.f32 %\[_a-z0-9\]*\\) dfn_rf(?:;|\[\r\n\]+\{)" 2 } } */
float dfn_rf ()
{
  return 5;
}

/* { dg-final { scan-assembler-times ".visible .func \\(.param.f64 %\[_a-z0-9\]*\\) dfn_rd(?:;|\[\r\n\]+\{)" 2 } } */
double dfn_rd ()
{
  return 6;
}

/* { dg-final { scan-assembler-times ".visible .func \\(.param.u64 %\[_a-z0-9\]*\\) dfn_rp(?:;|\[\r\n\]+\{)" 2 } } */
void *dfn_rp ()
{
  return 0;
}

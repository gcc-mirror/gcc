/* { dg-do compile } */
/* { dg-additional-options "-Wno-pedantic -Wno-long-long -m64" } */

/* K+R args.  as for ANSI except float promotes to double.  */

/* { dg-final { scan-assembler-times ".extern .func dcl_av;" 1 } } */
void dcl_av ();

/* { dg-final { scan-assembler-times ".extern .func dcl_ac \\(.param.u32 %\[_a-z0-9\]*\\);" 1 } } */
void dcl_ac ();

/* { dg-final { scan-assembler-times ".extern .func dcl_as \\(.param.u32 %\[_a-z0-9\]*\\);" 1 } } */
void dcl_as ();

/* { dg-final { scan-assembler-times ".extern .func dcl_ai \\(.param.u32 %\[_a-z0-9\]*\\);" 1 } } */
void dcl_ai ();

/* { dg-final { scan-assembler-times ".extern .func dcl_all \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
void dcl_all ();

/* { dg-final { scan-assembler-times ".extern .func dcl_af \\(.param.f64 %\[_a-z0-9\]*\\);" 1 } } */
void dcl_af ();

/* { dg-final { scan-assembler-times ".extern .func dcl_ad \\(.param.f64 %\[_a-z0-9\]*\\);" 1 } } */
void dcl_ad ();

/* { dg-final { scan-assembler-times ".extern .func dcl_ap \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
void dcl_ap ();

void test_1 (void)
{
  dcl_av ();
  dcl_ac ((char)1);
  dcl_as ((short)2);
  dcl_ai ((int)3);
  dcl_all ((long long)4);
  dcl_af ((float)5);
  dcl_ad ((double)6);
  dcl_ap ((void *)0);
}

/* { dg-final { scan-assembler-times ".visible .func dfn_av(?:;|\[\r\n\]+\{)" 2 } } */
void dfn_av ()
{
}

/* { dg-final { scan-assembler-times ".visible .func dfn_ac \\(.param.u32 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
void dfn_ac (c)
  char c;
{
}

/* { dg-final { scan-assembler-times ".visible .func dfn_as \\(.param.u32 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
void dfn_as (s)
  short s;
{
}

/* { dg-final { scan-assembler-times ".visible .func dfn_ai \\(.param.u32 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
void dfn_ai (i)
  int i;
{
}

/* { dg-final { scan-assembler-times ".visible .func dfn_all \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
void dfn_all (ll)
  long long ll;
{
}

/* { dg-final { scan-assembler-times ".visible .func dfn_af \\(.param.f64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
void dfn_af (f)
  float f;
{
}

/* { dg-final { scan-assembler-times ".visible .func dfn_ad \\(.param.f64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
void dfn_ad (d)
  double d;
{
}

/* { dg-final { scan-assembler-times ".visible .func dfn_ap \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
void dfn_ap (p)
  void *p;
{
}

/*  But complex float is passed as two floats.  (K&R doesn't have
    complex, so why obey k&r for the components of such an object?)  */

/* { dg-final { scan-assembler-times ".visible .func dfn_acf \\(.param.f32 %\[_a-z0-9\]*, .param.f32 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
void dfn_acf (_Complex float f)
{
}

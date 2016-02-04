/* { dg-do compile } */
/* { dg-additional-options "-Wno-pedantic -Wno-long-long -m64" } */

/* varargs, passed as pointer to array of args.   */

/* { dg-final { scan-assembler-times ".extern .func dcl_av \\(.param.u32 %\[_a-z0-9\]*, .param.u64 %\[_a-z0-9\]*\\);" 1 } } */
void dcl_av (int, ...);

void test_1 (void)
{
  dcl_av (1, 1);
  dcl_av (2, 1, 2);
  dcl_av (2, 1, 2, 3);
}

/* { dg-final { scan-assembler-times ".visible .func dfn_av \\(.param.u32 %\[_a-z0-9\]*, .param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
void dfn_av (int a, ...)
{
}

/* { dg-do compile } */
/* { dg-additional-options "-Wno-pedantic -Wno-long-long -m64 -fshort-enums" } */

/* Enum return types.  Passed as the underlying integer.  */

typedef enum { a = 0x1, b } Echar;
typedef enum { c = 0x100, d } Eshort;
typedef enum { e = 0x10000, f } Eint;
typedef enum { g = 0x100000000LL, h } Elonglong;

/* { dg-final { scan-assembler-times ".extern .func dcl_ac \\(.param.u32 %\[_a-z0-9\]*\\);" 1 } } */
void dcl_ac (Echar);

/* { dg-final { scan-assembler-times ".extern .func dcl_as \\(.param.u32 %\[_a-z0-9\]*\\);" 1 } } */
void dcl_as (Eshort);

/* { dg-final { scan-assembler-times ".extern .func dcl_ai \\(.param.u32 %\[_a-z0-9\]*\\);" 1 } } */
void dcl_ai (Eint);

/* { dg-final { scan-assembler-times ".extern .func dcl_all \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
void dcl_all (Elonglong);

void test_1 (void)
{
  dcl_ac (1);
  dcl_as (2);
  dcl_ai (3);
  dcl_all (4);
}

/* { dg-final { scan-assembler-times ".visible .func dfn_ac \\(.param.u32 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
void dfn_ac (Echar c)
{
}

/* { dg-final { scan-assembler-times ".visible .func dfn_as \\(.param.u32 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
void dfn_as (Eshort s)
{
}

/* { dg-final { scan-assembler-times ".visible .func dfn_ai \\(.param.u32 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
void dfn_ai (Eint i)
{
}

/* { dg-final { scan-assembler-times ".visible .func dfn_all \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
void dfn_all (Elonglong ll)
{
}

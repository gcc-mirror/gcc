/* { dg-do compile } */
/* { dg-additional-options "-Wno-pedantic -Wno-long-long -m64 -fshort-enums" } */

/* Enum return types.  Passed as the underlying integer.  */

typedef enum { a = 0x1, b } Echar;
typedef enum { c = 0x100, d } Eshort;
typedef enum { e = 0x10000, f } Eint;
typedef enum { g = 0x100000000LL, h } Elonglong;

/* { dg-final { scan-assembler-times ".extern .func \\(.param.u32 %\[_a-z\]*\\) dcl_rc;" 1 } } */
Echar dcl_rc (void);

/* { dg-final { scan-assembler-times ".extern .func \\(.param.u32 %\[_a-z\]*\\) dcl_rs;" 1 } } */
Eshort dcl_rs (void);

/* { dg-final { scan-assembler-times ".extern .func \\(.param.u32 %\[_a-z\]*\\) dcl_ri;" 1 } } */
Eint dcl_ri (void);

/* { dg-final { scan-assembler-times ".extern .func \\(.param.u64 %\[_a-z\]*\\) dcl_rll;" 1 } } */
Elonglong dcl_rll (void);

void test_1 (void)
{
  dcl_rc ();
  dcl_rs ();
  dcl_ri ();
  dcl_rll ();
}

/* { dg-final { scan-assembler-times ".visible .func \\(.param.u32 %\[_a-z0-9\]*\\) dfn_rc(?:;|\[\r\n\]+\{)" 2 } } */
Echar dfn_rc (void)
{
  return 1;
}

/* { dg-final { scan-assembler-times ".visible .func \\(.param.u32 %\[_a-z0-0\]*\\) dfn_rs(?:;|\[\r\n\]+\{)" 2 } } */
Eshort dfn_rs (void)
{
  return 2;
}

/* { dg-final { scan-assembler-times ".visible .func \\(.param.u32 %\[_a-z0-9\]*\\) dfn_ri(?:;|\[\r\n\]+\{)" 2 } } */
Eint dfn_ri (void)
{
  return 3;
}

/* { dg-final { scan-assembler-times ".visible .func \\(.param.u64 %\[_a-z0-9\]*\\) dfn_rll(?:;|\[\r\n\]+\{)" 2 } } */
Elonglong dfn_rll (void)
{
  return 4;
}

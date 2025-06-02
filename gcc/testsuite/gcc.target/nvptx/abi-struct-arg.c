/* { dg-do compile } */
/* { dg-additional-options "-Wno-pedantic -Wno-long-long -m64" } */

/* Struct arg.  Passed via pointer.  */

typedef struct {} empty;  /* See 'gcc/doc/extend.texi', "Empty Structures".  */
typedef struct {char a;} one;
typedef struct {short a;} two;
typedef struct {int a;} four;
typedef struct {long long a;} eight;
typedef struct {int a, b[12];} big;

/* { dg-final { scan-assembler-times ".extern .func dcl_aempty \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
void dcl_aempty (empty);

/* { dg-final { scan-assembler-times ".extern .func dcl_aone \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
void dcl_aone (one);

/* { dg-final { scan-assembler-times ".extern .func dcl_atwo \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
void dcl_atwo (two);

/* { dg-final { scan-assembler-times ".extern .func dcl_afour \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
void dcl_afour (four);

/* { dg-final { scan-assembler-times ".extern .func dcl_aeight \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
void dcl_aeight (eight);

/* { dg-final { scan-assembler-times ".extern .func dcl_abig \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
void dcl_abig (big);

#define M(T, v) ({T t; t.a = v; t;})

void test_1 (void)
{
  dcl_aempty (({empty t; t;}));
  dcl_aone (M (one, 1));
  dcl_atwo (M (two, 2));
  dcl_afour (M (four, 3));
  dcl_aeight (M (eight, 4));
  dcl_abig (M (big, 5));
}

/* { dg-final { scan-assembler-times ".visible .func dfn_aempty \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
void dfn_aempty (empty empty)
{
}

/* { dg-final { scan-assembler-times ".visible .func dfn_aone \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
void dfn_aone (one one)
{
}

/* { dg-final { scan-assembler-times ".visible .func dfn_atwo \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
void dfn_atwo (two two)
{
}

/* { dg-final { scan-assembler-times ".visible .func dfn_afour \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
void dfn_afour (four four)
{
}

/* { dg-final { scan-assembler-times ".visible .func dfn_aeight \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
void dfn_aeight (eight eight)
{
}

/* { dg-final { scan-assembler-times ".visible .func dfn_abig \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
void dfn_abig (big big)
{
}


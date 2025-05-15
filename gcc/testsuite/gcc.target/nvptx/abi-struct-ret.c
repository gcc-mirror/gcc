/* { dg-do compile } */
/* { dg-additional-options "-Wno-pedantic -Wno-long-long -m64" } */

/* Struct return.  Returned via pointer.  */

typedef struct {} empty;  /* See 'gcc/doc/extend.texi', "Empty Structures".  */
typedef struct {char a;} one;
typedef struct {short a;} two;
typedef struct {int a;} four;
typedef struct {long long a;} eight;
typedef struct {int a, b[12];} big;

/* { dg-final { scan-assembler-times ".extern .func dcl_rempty \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
empty dcl_rempty (void);

/* { dg-final { scan-assembler-times ".extern .func dcl_rone \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
one dcl_rone (void);

/* { dg-final { scan-assembler-times ".extern .func dcl_rtwo \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
two dcl_rtwo (void);

/* { dg-final { scan-assembler-times ".extern .func dcl_rfour \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
four dcl_rfour (void);

/* { dg-final { scan-assembler-times ".extern .func dcl_reight \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
eight dcl_reight (void);

/* { dg-final { scan-assembler-times ".extern .func dcl_rbig \\(.param.u64 %\[_a-z0-9\]*\\);" 1 } } */
big dcl_rbig (void);

void test_1 (void)
{
  dcl_rempty ();
  dcl_rone ();
  dcl_rtwo ();
  dcl_rfour ();
  dcl_reight ();
  dcl_rbig ();
}

#define M(T, v) ({T t; t.a = v; t;})

/* { dg-final { scan-assembler-times ".visible .func dfn_rempty \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
empty dfn_rempty (void)
{
  return ({empty t; t;});
}

/* { dg-final { scan-assembler-times ".visible .func dfn_rone \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
one dfn_rone (void)
{
  return M (one, 1);
}

/* { dg-final { scan-assembler-times ".visible .func dfn_rtwo \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
two dfn_rtwo (void)
{
  return M (two, 2);
}

/* { dg-final { scan-assembler-times ".visible .func dfn_rfour \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
four dfn_rfour (void)
{
  return M (four, 3);
}

/* { dg-final { scan-assembler-times ".visible .func dfn_reight \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
eight dfn_reight (void)
{
  return M (eight, 4);
}

/* { dg-final { scan-assembler-times ".visible .func dfn_rbig \\(.param.u64 %\[_a-z0-9\]*\\)(?:;|\[\r\n\]+\{)" 2 } } */
big dfn_rbig (void)
{
  return M (big, 5);
}

/* { dg-do compile } */
/* { dg-options "-march=rv32imafdc_zfa -mabi=ilp32d" { target { rv32 } } } */
/* { dg-options "-march=rv64imafdc_zfa -mabi=lp64d" { target { rv64 } } } */

extern void abort(void);
extern float a, b;
extern double c, d;

void 
foo()
{
  if ((__builtin_isless(a, b) ||  __builtin_islessequal(c, d))
      && (__builtin_islessequal(a, b)|| __builtin_isless(c, d)))
    abort();
}

/* { dg-final { scan-assembler-times {\mfleq\.s\M} 1 } } */
/* { dg-final { scan-assembler-times {\mfltq\.s\M} 1 } } */
/* { dg-final { scan-assembler-times {\mfleq\.d\M} 1 } } */
/* { dg-final { scan-assembler-times {\mfltq\.d\M} 1 } } */

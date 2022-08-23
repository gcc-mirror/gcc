/* { dg-do compile } */
/* { dg-require-ifunc "" } */
/* { dg-options "-O2 -fpic" } */

__attribute__((target_clones("default","fma")))
static inline double
expfull_ref(double x)
{
  return __builtin_pow(x, 0.1234);
}

double
exp_ref(double x)
{
  return expfull_ref(x);
}

/* { dg-final { scan-assembler "jmp\[ \t\]*expfull_ref@PLT" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "call\[ \t\]*expfull_ref@PLT" { target ia32 } } } */

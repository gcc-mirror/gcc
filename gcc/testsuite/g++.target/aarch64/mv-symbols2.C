/* { dg-do compile } */
/* { dg-require-ifunc "" } */
/* { dg-options "-O0" } */

__attribute__((target_version("default")))
int foo ()
{
  return 1;
}

__attribute__((target_version("dotprod")))
int foo ()
{
  return 3;
}
__attribute__((target_version("sve+sve2")))
int foo ()
{
  return 5;
}

__attribute__((target_version("sve+sve2")))
int foo (int)
{
  return 6;
}

__attribute__((target_version("dotprod")))
int foo (int)
{
  return 4;
}

__attribute__((target_version("default")))
int foo (int)
{
  return 2;
}

/* { dg-final { scan-assembler-times "\n_Z3foov\.default:\n" 1 } } */
/* { dg-final { scan-assembler-times "\n_Z3foov\._Mdotprod:\n" 1 } } */
/* { dg-final { scan-assembler-times "\n_Z3foov\._MsveMsve2:\n" 1 } } */
/* { dg-final { scan-assembler-times "\n_Z3foov\.resolver:\n" 0 } } */
/* { dg-final { scan-assembler-times "\n\t\.type\t_Z3foov, %gnu_indirect_function\n" 0 } } */
/* { dg-final { scan-assembler-times "\n\t\.set\t_Z3foov,_Z3foov\.resolver\n" 0 } } */

/* { dg-final { scan-assembler-times "\n_Z3fooi\.default:\n" 1 } } */
/* { dg-final { scan-assembler-times "\n_Z3fooi\._Mdotprod:\n" 1 } } */
/* { dg-final { scan-assembler-times "\n_Z3fooi\._MsveMsve2:\n" 1 } } */
/* { dg-final { scan-assembler-times "\n_Z3fooi\.resolver:\n" 0 } } */
/* { dg-final { scan-assembler-times "\n\t\.type\t_Z3fooi, %gnu_indirect_function\n" 0 } } */
/* { dg-final { scan-assembler-times "\n\t\.set\t_Z3fooi,_Z3fooi\.resolver\n" 0 } } */

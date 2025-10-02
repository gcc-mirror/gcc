/* { dg-do compile } */
/* { dg-options "-O0" } */

__attribute__ ((target_version ("default"))) int
foo ()
{
  return 1;
}

int bar()
{
  return foo();
}

/* { dg-final { scan-assembler-times "\n_Z3foov:\n" 0 } } */
/* { dg-final { scan-assembler-times "\n_Z3foov\.default:\n" 1 } } */
/* { dg-final { scan-assembler-times "\n_Z3foov\.resolver:\n" 0 } } */
/* { dg-final { scan-assembler-times "bl\t_Z3foov.default\n" 1 } } */
/* { dg-final { scan-assembler-times ".global\t_Z3foov\n" 1 } } */
/* { dg-final { scan-assembler-times ".set\t_Z3foov,_Z3foov.default\n" 1 } } */

/* { dg-do compile } */
/* { dg-options "-O0" } */

int foo () {
  return 1;
}

void bar ()
{
  int (*test)() = foo;

  test();
}

__attribute__ ((target_version ("dotprod"))) int foo ();

/* { dg-final { scan-assembler-times "\n_Z3foov\.default:\n" 1 } } */
/* { dg-final { scan-assembler-times "\n_Z3foov\._Mdotprod:\n" 0 } } */

/* { dg-final { scan-assembler-times "\n_Z3foov\.resolver:\n" 1 } } */
/* { dg-final { scan-assembler-times "\n\tadrp\tx\[0-9\]+, _Z3foov\._Mdotprod\n" 1 } } */
/* { dg-final { scan-assembler-times "\n\tadrp\tx\[0-9\]+, _Z3foov\.default\n" 1 } } */

/* { dg-final { scan-assembler-times "\n\tadrp\tx\[0-9\]+, _Z3foov\n" 1 } } */

/* { dg-final { scan-assembler-times "\n\t\.type\t_Z3foov, %gnu_indirect_function\n" 1 } } */
/* { dg-final { scan-assembler-times "\n\t\.set\t_Z3foov,_Z3foov\.resolver\n" 1 } } */

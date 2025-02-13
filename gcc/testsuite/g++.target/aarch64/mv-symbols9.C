/* { dg-do compile } */
/* { dg-options "-O0" } */

int
foo ();

int
foo ()
{
  return 1;
}

__attribute__ ((target_version ("dotprod"))) int
foo ()
{
  return 3;
}
__attribute__ ((target_version ("sve+sve2"))) int
foo ()
{
  return 5;
}

int
bar ()
{
  return foo ();
}

/* { dg-final { scan-assembler-times "\n_Z3foov\.default:\n" 1 } } */
/* { dg-final { scan-assembler-times "\n_Z3foov\._Mdotprod:\n" 1 } } */
/* { dg-final { scan-assembler-times "\n_Z3foov\._MsveMsve2:\n" 1 } } */

/* { dg-final { scan-assembler-times "\n_Z3foov\.resolver:\n" 1 } } */
/* { dg-final { scan-assembler-times "\n\tadrp\tx\[0-9\]+, _Z3foov\._MsveMsve2\n" 1 } } */
/* { dg-final { scan-assembler-times "\n\tadrp\tx\[0-9\]+, _Z3foov\._Mdotprod\n" 1 } } */
/* { dg-final { scan-assembler-times "\n\tadrp\tx\[0-9\]+, _Z3foov\.default\n" 1 } } */

/* { dg-final { scan-assembler-times "\n\tbl\t_Z3foov\n" 1 } } */

/* { dg-final { scan-assembler-times "\n\t\.type\t_Z3foov, %gnu_indirect_function\n" 1 } } */
/* { dg-final { scan-assembler-times "\n\t\.set\t_Z3foov,_Z3foov\.resolver\n" 1 } } */

/* { dg-do compile } */
/* { dg-require-ifunc "" } */
/* { dg-options "-O0" } */

__attribute__((target_clones("dotprod", "sve+sve2")))
int foo ();

__attribute__((target_version("default")))
int foo ()
{
  return 0;
}

__attribute__((target_clones("sve2+sme", "sve2+sme2")))
int foo ()
{
  return 2;
}

int bar()
{
  return foo ();
}


/* { dg-final { scan-assembler-times "\n_Z3foov\.default:\n" 1 } } */
/* { dg-final { scan-assembler-times "\n_Z3foov\._Mdotprod:\n" 0 } } */
/* { dg-final { scan-assembler-times "\n_Z3foov\._MsveMsve2:\n" 0 } } */
/* { dg-final { scan-assembler-times "\n_Z3foov\._Msve2Msme:\n" 1 } } */
/* { dg-final { scan-assembler-times "\n_Z3foov\._Msve2Msme2:\n" 1 } } */
/* { dg-final { scan-assembler-times "\n_Z3foov\.resolver:\n" 1 } } */
/* { dg-final { scan-assembler-times "\n\tbl\t_Z3foov\n" 1 } } */
/* { dg-final { scan-assembler-times "\n\t\.type\t_Z3foov, %gnu_indirect_function\n" 1 } } */
/* { dg-final { scan-assembler-times "\n\t\.set\t_Z3foov,_Z3foov\.resolver\n" 1 } } */
// { dg-final { scan-assembler-times "\n\tadrp\tx\[0-9\]+, _Z3foov\.default\n" 1 } }
/* { dg-final { scan-assembler-times "\n\tadrp\tx\[0-9\]+, _Z3foov\._Mdotprod\n" 1 } } */
/* { dg-final { scan-assembler-times "\n\tadrp\tx\[0-9\]+, _Z3foov\._MsveMsve2\n" 1 } } */
/* { dg-final { scan-assembler-times "\n\tadrp\tx\[0-9\]+, _Z3foov\._Msve2Msme\n" 1 } } */
/* { dg-final { scan-assembler-times "\n\tadrp\tx\[0-9\]+, _Z3foov\._Msve2Msme2\n" 1 } } */


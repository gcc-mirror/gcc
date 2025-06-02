/* { dg-do compile } */
/* { dg-require-ifunc "" } */
/* { dg-options "-O0" } */

__attribute__((target_clones("default", "cpu=power6", "cpu=power6x")))
int foo ()
{
  return 1;
}

__attribute__((target_clones("cpu=power6x", "cpu=power6", "default")))
int foo (int)
{
  return 2;
}

int bar()
{
  return foo ();
}

int bar(int x)
{
  return foo (x);
}

/* { dg-final { scan-assembler-times "\n_Z3foov\.default:\n" 1 } } */
/* { dg-final { scan-assembler-times "\n_Z3foov\.cpu_power6:\n" 1 } } */
/* { dg-final { scan-assembler-times "\n_Z3foov\.cpu_power6x:\n" 1 } } */
/* { dg-final { scan-assembler-times "\n_Z3foov\.resolver:\n" 1 } } */
/* { dg-final { scan-assembler-times "\n\tbl _Z3foov\n" 1 } } */
/* { dg-final { scan-assembler-times "\n\t\.type\t_Z3foov, @gnu_indirect_function\n" 1 } } */
/* { dg-final { scan-assembler-times "\n\t\.set\t_Z3foov,_Z3foov\.resolver\n" 1 } } */
/* { dg-final { scan-assembler-times "\n\t\.quad\t_Z3foov\.default\n" 1 } } */
/* { dg-final { scan-assembler-times "\n\t\.quad\t_Z3foov\.cpu_power6\n" 1 } } */
/* { dg-final { scan-assembler-times "\n\t\.quad\t_Z3foov\.cpu_power6x\n" 0 } } */

/* { dg-final { scan-assembler-times "\n_Z3fooi\.default:\n" 1 } } */
/* { dg-final { scan-assembler-times "\n_Z3fooi\.cpu_power6:\n" 1 } } */
/* { dg-final { scan-assembler-times "\n_Z3fooi\.cpu_power6x:\n" 1 } } */
/* { dg-final { scan-assembler-times "\n_Z3fooi\.resolver:\n" 1 } } */
/* { dg-final { scan-assembler-times "\n\tbl _Z3fooi\n" 1 } } */
/* { dg-final { scan-assembler-times "\n\t\.type\t_Z3fooi, @gnu_indirect_function\n" 1 } } */
/* { dg-final { scan-assembler-times "\n\t\.set\t_Z3fooi,_Z3fooi\.resolver\n" 1 } } */
/* { dg-final { scan-assembler-times "\n\t\.quad\t_Z3fooi\.default\n" 1 } } */
/* { dg-final { scan-assembler-times "\n\t\.quad\t_Z3fooi\.cpu_power6\n" 0 } } */
/* { dg-final { scan-assembler-times "\n\t\.quad\t_Z3fooi\.cpu_power6x\n" 1 } } */

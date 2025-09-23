/* { dg-do compile } */
/* { dg-require-ifunc "" } */
/* { dg-options "-O0 -march=loongarch64 -mabi=lp64d -mcmodel=normal" } */
/* { dg-final { scan-assembler-times "\n_Z3foov\.default:\n" 1 } } */
/* { dg-final { scan-assembler-times "\n_Z3foov\.arch_la64v1_1:\n" 1 } } */
/* { dg-final { scan-assembler-times "\n_Z3foov\.lsx:\n" 1 } } */
/* { dg-final { scan-assembler-times "\n_Z3foov\.resolver:\n" 1 } } */
/* { dg-final { scan-assembler-times "\tbl\t\\\%plt\\\(_Z3foov\\\)\n" 1 } } */
/* { dg-final { scan-assembler-times "\t\.type\t_Z3foov, @gnu_indirect_function" 1 } } */
/* { dg-final { scan-assembler-times "\t\.set\t_Z3foov,_Z3foov\.resolver" 1 } } */

/* { dg-final { scan-assembler-times "\n_Z3fooi\.default:\n" 1 } } */
/* { dg-final { scan-assembler-times "\n_Z3fooi\.arch_la64v1_1:\n" 1 } } */
/* { dg-final { scan-assembler-times "\n_Z3fooi\.lsx:\n" 1 } } */
/* { dg-final { scan-assembler-times "\n_Z3fooi\.resolver:\n" 1 } } */
/* { dg-final { scan-assembler-times "\tbl\t\\\%plt\\\(_Z3fooi\\\)\n" 1 } } */
/* { dg-final { scan-assembler-times "\t\.type\t_Z3fooi, @gnu_indirect_function" 1 } } */
/* { dg-final { scan-assembler-times "\t\.set\t_Z3fooi,_Z3fooi\.resolver" 1 } } */

__attribute__((target_clones("default", "arch=la64v1.1", "lsx")))
int foo ()
{
  return 1;
}

__attribute__((target_clones("default", "arch=la64v1.1", "lsx")))
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


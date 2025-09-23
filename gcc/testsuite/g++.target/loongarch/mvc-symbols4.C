/* { dg-do compile } */
/* { dg-require-ifunc "" } */
/* { dg-options "-O0 -march=loongarch64 -mabi=lp64d -mcmodel=normal" } */
/* { dg-final { scan-assembler-not "\n_Z3foov\.default:\n" } } */
/* { dg-final { scan-assembler-not "\n_Z3foov\.arch_la64v1_1:\n" } } */
/* { dg-final { scan-assembler-not "\n_Z3foov\.lsx:\n" } } */
/* { dg-final { scan-assembler-not "\n_Z3foov\.resolver:\n" } } */
/* { dg-final { scan-assembler-not "\tbl\t\\\%plt\\\(_Z3foov\\\)\n" } } */
/* { dg-final { scan-assembler-not "\t\.type\t_Z3foov, @gnu_indirect_function" } } */
/* { dg-final { scan-assembler-not "\t\.set\t_Z3foov,_Z3foov\.resolver" } } */

/* { dg-final { scan-assembler-not "\n_Z3fooi\.default:\n" } } */
/* { dg-final { scan-assembler-not "\n_Z3fooi\.arch_la64v1_1:\n" } } */
/* { dg-final { scan-assembler-not "\n_Z3fooi\.lsx:\n" } } */
/* { dg-final { scan-assembler-not "\n_Z3fooi\.resolver:\n" } } */
/* { dg-final { scan-assembler-not "\tbl\t\\\%plt\\\(_Z3fooi\\\)\n" } } */
/* { dg-final { scan-assembler-not "\t\.type\t_Z3fooi, @gnu_indirect_function" } } */
/* { dg-final { scan-assembler-not "\t\.set\t_Z3fooi,_Z3fooi\.resolver" } } */

__attribute__((target_clones("default", "arch=la64v1.1", "lsx")))
int foo ();

__attribute__((target_clones("default", "arch=la64v1.1", "lsx")))
int foo (int);



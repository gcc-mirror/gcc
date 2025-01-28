/* { dg-do compile } */
/* { dg-require-ifunc "" } */
/* { dg-options "-O0" } */

__attribute__((target_clones("default", "cpu=power6", "cpu=power6x")))
int foo ();

__attribute__((target_clones("cpu=power6x", "cpu=power6", "default")))
int foo (int);

/* { dg-final { scan-assembler-times "\n_Z3foov\.default:\n" 0 } } */
/* { dg-final { scan-assembler-times "\n_Z3foov\.cpu_power6:\n" 0 } } */
/* { dg-final { scan-assembler-times "\n_Z3foov\.cpu_power6x:\n" 0 } } */
/* { dg-final { scan-assembler-times "\n_Z3foov\.resolver:\n" 0 } } */
/* { dg-final { scan-assembler-times "\n\t\.type\t_Z3foov, @gnu_indirect_function\n" 0 } } */
/* { dg-final { scan-assembler-times "\n\t\.set\t_Z3foov,_Z3foov\.resolver\n" 0 } } */
/* { dg-final { scan-assembler-times "\n\tlis \[\\d\]+,_Z3foov\.default@ha\n" 0 } } */
/* { dg-final { scan-assembler-times "\n\tlis \[\\d\]+,_Z3foov\.cpu_power6@ha\n" 0 } } */
/* { dg-final { scan-assembler-times "\n\tlis \[\\d\]+,_Z3foov\.cpu_power6x@ha\n" 0 } } */

/* { dg-final { scan-assembler-times "\n_Z3fooi\.default:\n" 0 } } */
/* { dg-final { scan-assembler-times "\n_Z3fooi\.cpu_power6:\n" 0 } } */
/* { dg-final { scan-assembler-times "\n_Z3fooi\.cpu_power6x:\n" 0 } } */
/* { dg-final { scan-assembler-times "\n_Z3fooi\.resolver:\n" 0 } } */
/* { dg-final { scan-assembler-times "\n\t\.type\t_Z3fooi, @gnu_indirect_function\n" 0 } } */
/* { dg-final { scan-assembler-times "\n\t\.set\t_Z3fooi,_Z3fooi\.resolver\n" 0 } } */
/* { dg-final { scan-assembler-times "\n\tlis \[\\d\]+,_Z3fooi\.default@ha\n" 0 } } */
/* { dg-final { scan-assembler-times "\n\tlis \[\\d\]+,_Z3fooi\.cpu_power6@ha\n" 0 } } */
/* { dg-final { scan-assembler-times "\n\tlis \[\\d\]+,_Z3fooi\.cpu_power6x@ha\n" 0 } } */

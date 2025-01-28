/* { dg-do compile } */
/* { dg-require-ifunc "" } */
/* { dg-options "-O0" } */

__attribute__((target("default")))
int foo ();

__attribute__((target("arch=slm")))
int foo ()
{
  return 3;
}

__attribute__((target("sse4.2")))
int foo ()
{
  return 5;
}

__attribute__((target("sse4.2")))
int foo (int)
{
  return 6;
}

__attribute__((target("arch=slm")))
int foo (int)
{
  return 4;
}

__attribute__((target("default")))
int foo (int);

int bar()
{
  return foo ();
}

/* When updating any of the symbol names in these tests, make sure to also
   update any tests for their absence in mvc-symbolsN.C */

/* { dg-final { scan-assembler-times "\n_Z3foov:\n" 0 } } */
/* { dg-final { scan-assembler-times "\n_Z3foov\.arch_slm:\n" 1 } } */
/* { dg-final { scan-assembler-times "\n_Z3foov\.sse4.2:\n" 1 } } */
/* { dg-final { scan-assembler-times "\n_Z3foov\.resolver:\n" 1 } } */
/* { dg-final { scan-assembler-times "\n\tcall\t_Z7_Z3foovv\n" 1 } } */
/* { dg-final { scan-assembler-times "\n\t\.type\t_Z7_Z3foovv, @gnu_indirect_function\n" 1 } } */
/* { dg-final { scan-assembler-times "\n\t\.set\t_Z7_Z3foovv,_Z3foov\.resolver\n" 1 } } */

/* { dg-final { scan-assembler-times "\n_Z3fooi:\n" 0 } } */
/* { dg-final { scan-assembler-times "\n_Z3fooi\.arch_slm:\n" 1 } } */
/* { dg-final { scan-assembler-times "\n_Z3fooi\.sse4.2:\n" 1 } } */
/* { dg-final { scan-assembler-times "\n_Z3fooi\.resolver:\n" 0 } } */
/* { dg-final { scan-assembler-times "\n\t\.type\t_Z7_Z3fooii, @gnu_indirect_function\n" 0 } } */
/* { dg-final { scan-assembler-times "\n\t\.set\t_Z7_Z3fooii,_Z3fooi\.resolver\n" 0 } } */

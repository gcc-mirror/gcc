/* PR ipa/81214.  */
/* { dg-do compile } */
/* { dg-require-ifunc "" } */

__attribute__((target_clones("avx","arch=slm","arch=core-avx2","default")))
int
foo ()
{
  return -2;
}

/* { dg-final { scan-assembler "\t.globl\tfoo" } } */
/* { dg-final { scan-assembler "foo.resolver:" } } */
/* { dg-final { scan-assembler "foo, @gnu_indirect_function" } } */

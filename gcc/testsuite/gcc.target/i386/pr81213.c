/* PR ipa/81214.  */
/* { dg-do compile } */
/* { dg-require-ifunc "" } */

__attribute__((target_clones("avx","arch=slm","arch=core-avx2","default")))
static int
foo ()
{
  return -2;
}

int main()
{
  return foo();
}

/* { dg-final { scan-assembler "\t.globl\tfoo\\..*\\.ifunc" } } */
/* { dg-final { scan-assembler "foo.resolver:" } } */
/* { dg-final { scan-assembler "foo\\..*\\.ifunc, @gnu_indirect_function" } } */

/* { dg-do compile } */
/* { dg-require-ifunc "" } */
/* { dg-final { scan-assembler "foo.resolver" } } */
/* { dg-final { scan-assembler "avx" } } */
/* { dg-final { scan-assembler "slm" } } */
/* { dg-final { scan-assembler-times "foo.ifunc" 4 } } */

__attribute__((target_clones("avx","default","arch=slm","arch=core-avx2")))
int foo ();

int main()
{
  return foo();
}

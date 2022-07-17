/* { dg-do compile } */
/* { dg-require-ifunc "" } */

/* Verify that foo clones are not numbered.  */
/* { dg-final { scan-assembler "foo\.resolver," } } */
/* { dg-final { scan-assembler "foo\.default\[,@\]" } } */
/* { dg-final { scan-assembler "foo\.avx\[,@\]" } } */
/* { dg-final { scan-assembler "foo\.arch_core_avx2\[,@\]" } } */
/* { dg-final { scan-assembler "foo\.arch_slm\[,@\]" } } */
/* { dg-final { scan-assembler "foo,foo\.resolver" } } */

__attribute__((target_clones("avx","default","arch=slm","arch=core-avx2")))
int foo ();

int main()
{
  return foo();
}

/* { dg-do compile } */
/* { dg-require-ifunc "" } */
/* { dg-options "-fno-inline" } */
/* { dg-final { scan-assembler "foo,foo.resolver" } } */

__attribute__((target_clones("default","avx","avx2")))
int
foo ()
{
  return 10;
}

__attribute__((target_clones("default","avx","avx2")))
int
bar ()
{
  return -foo ();
}

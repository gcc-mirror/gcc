/* { dg-do compile } */
/* { dg-options "-fcf-protection" } */
/* { dg-require-ifunc "" } */

__attribute__((target_clones("avx","arch=slm","arch=core-avx2","default")))
int
foo ()
{
  return -2;
}

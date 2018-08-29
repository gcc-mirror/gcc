/* { dg-do compile } */
/* { dg-require-ifunc "" } */

__attribute__((target_clones("","arch=slm","arch=core-avx2", "default")))
int foo (); /* { dg-error "an empty string cannot be in .target_clones. attribute" } */

int
bar ()
{
  return foo();
}

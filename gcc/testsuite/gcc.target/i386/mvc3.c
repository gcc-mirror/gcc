/* { dg-do compile } */

__attribute__((target_clones("avx","arch=slm","arch=core-avx2")))
int foo (); /* { dg-error "default target was not set" } */

int
bar ()
{
  return foo();
}

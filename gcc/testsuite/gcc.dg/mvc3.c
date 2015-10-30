/* { dg-do compile { target i?86-*-* x86_64-*-* } } */

__attribute__((target_clones("avx","arch=slm","arch=core-avx2")))
int foo (); /* { dg-error "default target was not set" } */

int
bar ()
{
  return foo();
}

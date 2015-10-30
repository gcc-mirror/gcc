/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-final { scan-assembler-times "foo.ifunc" 4 } } */

__attribute__((target_clones("avx","default","arch=slm","arch=core-avx2")))
int foo ();

int main()
{
  return foo();
}

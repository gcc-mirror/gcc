/* PR target/58372 */
/* { dg-do compile { target { i?86-*-* x86_64-*-* } } } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target c++14 } */

__attribute__ ((__target__ ("rdrnd")))
void f (unsigned int *b) noexcept
{
  __builtin_ia32_rdrand32_step (b);
}

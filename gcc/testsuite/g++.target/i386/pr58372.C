/* PR target/58372 */
/* { dg-do compile { target c++14 } } */
/* { dg-options "-O2" } */

__attribute__ ((__target__ ("rdrnd")))
void f (unsigned int *b) noexcept
{
  __builtin_ia32_rdrand32_step (b);
}

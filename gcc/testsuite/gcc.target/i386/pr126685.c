/* PR target/126685 */
/* { dg-do assemble { target ia32 } } */
/* { dg-options "-march=i386" } */

unsigned long long
foo (unsigned long long x)
{
  return __builtin_bswap64 (x);
}

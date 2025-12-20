/* PR target/123217 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-musermsr -O0" } */

unsigned long long
foo (unsigned long long x)
{
  unsigned long long y = __builtin_ia32_urdmsr (x);
  return y;
}

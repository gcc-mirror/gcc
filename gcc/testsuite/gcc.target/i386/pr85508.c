/* PR target/85508 */
/* { dg-do compile } */
/* { dg-options "-O2 -msse2" } */

typedef signed char V __attribute__((vector_size (16)));
signed char c;

V
foo (void)
{
  return (V) { c, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 };
}

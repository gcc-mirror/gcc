/* PR debug/104557 */
/* { dg-do compile } */
/* { dg-options "-O -g -Wno-psabi" } */

typedef int __attribute__((__vector_size__ (32))) U;
typedef double __attribute__((__vector_size__ (32))) F;
typedef _Decimal64 __attribute__((__vector_size__ (32))) D;

F
bar (void)
{
  F f = __builtin_convertvector ((D) (-10.d < (D) ((D) (U) { 0, 0, 0, 0, 0, 0, 0, -0xe0 }
						   >= (D) { 80000000 })), F);
  return f;
}

F
foo ()
{
  F x = bar ();
  return x;
}

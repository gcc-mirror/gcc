/* PR target/112394 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-msse4.1 -mno-sse4.2 -m64 -O -mapxf" } */

typedef int __attribute__((__vector_size__ (8))) A;
typedef int __attribute__((__vector_size__ (16))) B;
typedef char __attribute__((__vector_size__ (4))) C;
typedef char __attribute__((__vector_size__ (32))) D;
typedef _Complex __int128 CU;
typedef _Float16 __attribute__((__vector_size__ (8))) F;
D d;
B b;
CU gcu;

int
foo (char c, int, int, int, int, CU cu, int x)
{
  d /= c | d;
  F f = __builtin_convertvector (b, F);
  cu /= gcu;
  A a = (A) f;
  int i = cu + x;
  return ((C) a[0])[1] + i + c;
}

/* PR target/104469 */
/* { dg-do compile } */
/* { dg-options "-mavx512f" } */

typedef double __attribute__((__vector_size__ (64))) F;
typedef int __attribute__((__vector_size__ (32))) V;

F
foo (V v)
{
  return __builtin_convertvector (v, F);
}

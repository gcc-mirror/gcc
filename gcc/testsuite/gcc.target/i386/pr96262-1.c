/* PR target/96262 */
/* { dg-do compile } */
/* { dg-options "-mavx512bw -O" } */

typedef char __attribute__ ((__vector_size__ (64))) V;

V
foo (V v)
{
  return ~(v << 1);
}

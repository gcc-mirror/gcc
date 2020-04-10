/* PR target/94438 */
/* { dg-do compile { target int128 } } */
/* { dg-options "-mavx512bw" } */

typedef __attribute__ ((__vector_size__ (4 * sizeof (__int128)))) __int128 V;
void bar (V);

void
foo (V w)
{
  V v = 0 <= (0 >= w);
  bar (v);
}

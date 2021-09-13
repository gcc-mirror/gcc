/* PR target/101286 */
/* { dg-do compile { target int128 } } */
/* { dg-options "-mavx2" } */

typedef __attribute__((__vector_size__ (2 * sizeof (__int128)))) __int128 V;

V
foo (void)
{
  return (V){(__int128) 1 << 64 | 1, (__int128) 1 << 64 | 1};
}

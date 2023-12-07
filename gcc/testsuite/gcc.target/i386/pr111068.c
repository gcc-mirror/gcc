/* PR target/111068 */
/* { dg-do compile } */
/* { dg-options "-ffloat-store -mavx10.1-512" } */

typedef _Float16 __attribute__((__vector_size__ (8))) V;
V u, v, w;

void
foo (void)
{
  v /= __builtin_shufflevector (w, u, 3, 3, 6, 1);
}

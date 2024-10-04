/* PR target/116921 */
/* { dg-do compile { target int128 } } */
/* { dg-options "-O2 -msse4" } */

long x;
_Float16 __attribute__((__vector_size__ (16))) f;

void
foo (void)
{
  x -= !(__int128) (f / 2);
}

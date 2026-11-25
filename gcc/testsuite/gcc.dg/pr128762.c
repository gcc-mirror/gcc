/* PR target/128762 */
/* { dg-do compile { target int128 } } */
/* { dg-options "-O2" } */

__attribute__((__vector_size__(16))) short v;
__int128 x;
short s;

void
foo (void)
{
  __sync_bool_compare_and_swap (&x, v + s, 0);
}

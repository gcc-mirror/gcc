/* PR target/105825 */
/* { dg-do compile { target int128 } } */
/* { dg-options "-O2" } */
/* { dg-additional-options "-mavx" { target avx } } */

__int128 j;
int i;

void
foo (void)
{
  j <<= __builtin_parityll (i);
}

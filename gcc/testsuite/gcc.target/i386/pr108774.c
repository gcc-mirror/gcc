/* PR target/108774 */
/* { dg-do compile  { target lp64 } } */
/* { dg-options "-Os -ftrapv -mcmodel=large" } */

int i, j;

void
foo (void)
{
  i = ((1 << j) - 1) >> j;
}

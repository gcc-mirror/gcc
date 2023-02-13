/* PR target/108774 */
/* { dg-do compile  { target x86_64-*-* } } */
/* { dg-options "-Os -ftrapv -mcmodel=large" } */

int i, j;

void
foo (void)
{
  i = ((1 << j) - 1) >> j;
}

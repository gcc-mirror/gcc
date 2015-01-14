/* PR middle-end/64391 */
/* { dg-do compile } */
/* { dg-options "-fgnu-tm" } */

void
foo (void)
{
#pragma GCC ivdep
  while (1);
}

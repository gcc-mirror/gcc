/* PR target/59575 */
/* { dg-do compile } */
/* { dg-skip-if "-mpure-code supports M-profile only" { *-*-* } { "-mpure-code" } } */
/* { dg-options "-Os -g -march=armv7-a" } */

void foo (int *);
int *bar (int, long long, int);

void
test (int *p)
{
  if (p)
    foo (p);
  else if (p = bar (0, 1, 2))
    foo (p);
}

/* PR target/59575 */
/* { dg-do compile } */
/* { dg-require-effective-target arm_arch_v7a_ok } */
/* { dg-skip-if "-mpure-code supports M-profile only" { *-*-* } { "-mpure-code" } } */
/* { dg-options "-Os -g" } */
/* { dg-add-options arm_arch_v7a } */

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

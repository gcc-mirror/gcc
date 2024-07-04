/* { dg-do compile } */
/* { dg-require-effective-target arm_arch_v7a_ok } */
/* { dg-skip-if "-mpure-code supports M-profile only" { *-*-* } { "-mpure-code" } } */
/* { dg-options "-O2" } */
/* { dg-add-options arm_arch_v7a } */


void foo (void);

void
bar (int x, int y)
{
  y = 9999;
  if (x & (1 << y))
    foo ();
}

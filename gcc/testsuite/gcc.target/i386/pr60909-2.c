/* { dg-do compile } */
/* { dg-options "-mrdseed" } */

extern void bar (int);

void
foo (unsigned *u)
{
  int i = __builtin_ia32_rdseed_si_step (u);
  bar (i);
}

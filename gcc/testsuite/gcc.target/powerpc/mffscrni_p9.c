/* { dg-do compile } */
/* { dg-options "-O2 -mdejagnu-cpu=power9" } */
/* { dg-final { scan-assembler-times {\mmffscrni\M} 1 } } */

void foo ()
{
  int val = 2;
  __builtin_set_fpscr_rn (val);
}

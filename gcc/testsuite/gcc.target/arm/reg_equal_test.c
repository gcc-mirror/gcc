/* { dg-do compile } */
/* { dg-options "-fgimple -O1 -fdump-rtl-expand" } */
/* { dg-require-effective-target arm_cpu_cortex_a53_ok } */
/* { dg-add-options arm_cpu_cortex_a53 } */

void __GIMPLE (ssa,startwith ("expand"))
x ()
{
  unsigned int d;

  __BB(2,guessed_local(1073741824)):
  d_1 = 3352447838u;
  return;
}

/* { dg-final { scan-rtl-dump "expr_list:REG_EQUAL \\(const_int -942519458" "expand" } } */

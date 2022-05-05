/* { dg-do compile } */
/* { dg-options "-fgimple -O1 -fdump-rtl-expand" } */
/* { dg-skip-if "" { ! { arm_thumb2_ok || arm_thumb1_movt_ok } } } */

void __GIMPLE (ssa,startwith ("expand"))
x ()
{
  unsigned int d;

  __BB(2,guessed_local(1073741824)):
  d_1 = 3352447838u;
  return;
}

/* { dg-final { scan-rtl-dump "expr_list:REG_EQUAL \\(const_int -942519458" "expand" } } */

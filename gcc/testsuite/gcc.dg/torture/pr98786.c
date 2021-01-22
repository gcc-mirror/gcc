/* { dg-do compile } */
/* { dg-additional-options "-fno-tree-dce" } */

void
func_30 (void);

int __attribute__ ((pure, returns_twice))
func_38 (int g_15, int p_39)
{
  return !!g_15 ? p_39 : 0;
}

void
func_26 (int func_26___trans_tmp_1)
{
  long int l_37 = 0;
  int __trans_tmp_1;

  func_26___trans_tmp_1 = func_38 (func_26___trans_tmp_1, 1);
  __trans_tmp_1 = func_38 (func_26___trans_tmp_1, l_37);
  l_37 = 1;
  func_30 ();
}

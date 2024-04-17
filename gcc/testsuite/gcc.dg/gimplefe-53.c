/* { dg-do compile } */
/* { dg-options "-fgimple" } */

int __GIMPLE
foo (int a, int b)
{
  int tem;
  tem = a __EXACT_DIV b;
  tem = tem __CEIL_DIV b;
  tem = tem __FLOOR_DIV b;
  tem = tem __ROUND_DIV b;
  tem = tem __FLOOR_MOD b;
  tem = tem __CEIL_MOD b;
  tem = tem __ROUND_MOD b;
  return tem;
}

/* { dg-do compile } */
/* { dg-options "-fgimple" } */

int __GIMPLE
foo (int a, int b)
{
  int tem;
  tem = a __ROTATE_LEFT b;
  tem = tem __ROTATE_RIGHT b;
  return tem;
}

/* PR tree-optimization/103245 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times " = ABSU_EXPR <v_\[0-9]*\\\(D\\\)>;" 1 "optimized" } } */

unsigned
f1 (int v)
{
  unsigned int d_6;
  int b_5;
  int a_4;
  _Bool _1;
  unsigned int v1_2;
  unsigned int _7;
  int _9;

  _1 = v < 0;
  a_4 = (int) _1;
  b_5 = -a_4;
  _9 = b_5 | 1;
  d_6 = (unsigned int) _9;
  v1_2 = (unsigned int) v;
  _7 = v1_2 * d_6;
  return _7;
}

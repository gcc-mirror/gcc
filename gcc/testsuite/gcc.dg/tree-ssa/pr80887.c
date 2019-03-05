/* { dg-do compile } */
/* { dg-options "-fgimple -O" } */
/* { dg-require-effective-target int32plus } */

int pos;
void __GIMPLE (startwith("fre"))
f()
{
  unsigned int t2;
  unsigned int t1;
  int a;
  unsigned int u;
  int _1;
  int _2;
  int _3;
  unsigned int _4;
  int _5;
  unsigned int _6;

bb_2:
  _1 = pos;
  _2 = _1 + 1;
  pos = _2;
  _3 = pos;
  _4 = (unsigned int) _3;
  u_9 = _4 + 4294967295u;
  a_10 = pos;
  _5 = a_10 + _Literal (int) -1;
  t1_11 = (unsigned int) _5;
  _6 = (unsigned int) a_10;
  t2_12 = _6 + 4294967294u;
  return;
}

/* { dg-options "-O -fgimple" } */

int __GIMPLE ()
p (int n)
{
  int _2;
  _2 = n_1(D) != 0 ? 2 : 0;
  return _2;
}

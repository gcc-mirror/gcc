/* { dg-do compile } */
/* { dg-options "-O2 -frounding-math" } */
/* { dg-add-options float16  } */
/* { dg-require-effective-target float16  } */

void f(_Float16*);
void
foo1 ()
{
  int t0 = 3967;
  int t1 = 3969;
  int t2 = 3971;
  int t3 = 3973;
  _Float16 tt[4];
  tt[0] = t0;
  tt[1] = t1;
  tt[2] = t2;
  tt[3] = t3;
  f(&tt[0]);
}

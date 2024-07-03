/* { dg-do compile } */
/* { dg-options "-O2 -frounding-math" } */

void f(float*);
void
foo1 ()
{
  long long t0 = __LONG_LONG_MAX__;
  long long t1 = __LONG_LONG_MAX__ - 1;
  float tt[2];
  tt[0] = t0;
  tt[1] = t1;
  f(&tt[0]);
}


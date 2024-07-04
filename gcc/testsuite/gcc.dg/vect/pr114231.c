/* { dg-do compile } */

void f(long*);
int ff[2];
void f2(long, long, unsigned long);
void k(unsigned long x, unsigned long y)
{
  long t = x >> ff[0];
  long t1 = ff[1];
  unsigned long t2 = y >> ff[0];
  f2(t1, t+t2, t2);
}

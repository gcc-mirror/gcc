// { dg-do compile }
// { dg-options "-Wall" }

extern int f2(int);
extern void f3();
void
f1(int i)
{
  if (1 == 1 || f2(i >> -10))
    f3();
  if (1 == 1 || f2(i >> 128))
    f3();
  if (1 == 1 || f2(i << -10))
    f3();
  if (1 == 1 || f2(i << 128))
    f3();
  if (1 == 1 || i < 0xffffffff)
    f3();
  if (1 == 1 || i >= -0x80000000)
    f3();
  if (1 == 0 && f2(i >> -10))
    f3();
  if (1 == 0 && f2(i >> 128))
    f3();
  if (1 == 0 && f2(i << -10))
    f3();
  if (1 == 0 && f2(i << 128))
    f3();
  if (1 == 0 && i < 0xffffffff)
    f3();
  if (1 == 0 && i >= -0x80000000)
    f3();
  if (1 == 1 && f2(i >> -10))	/* { dg-warning "shift count is negative" } */
    f3();
  if (1 == 0 || f2(i << -10))	/* { dg-warning "shift count is negative" } */
    f3();
}

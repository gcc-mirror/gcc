/* { dg-options "-mthumb" { target arm*-*-* } } */
/* { dg-do run } */
/* { dg-timeout 100 } */

/* PR middle-end/39378 */
/* Check if the thunk target function is emitted correctly. */
class B1
{
public:
  virtual int foo1(void);
  int b1;
};

class B2
{
public:
  virtual int foo2 (void);
  int b2;
};

class D : public B1, public B2
{
  int foo1(void);
  int foo2(void);
  int d;
};

int B1::foo1(void)
{
  return 3;
}

int B2::foo2(void)
{
  return 4;
}

int D::foo1(void)
{
  return 1;
}

int D::foo2(void)
{
  return 2;
}

__attribute__((noinline)) int test(B2* bp)
{
  return bp->foo2();
}

int main()
{
  B2 *bp = new D();
  if (test(bp) == 2)
    return 0;
  else
    return 1;
}

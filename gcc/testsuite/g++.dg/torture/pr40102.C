/* { dg-do compile } */
bool foo0(int) { return true; }

bool foo1();

struct A
{
  A();
  ~A();

  template<typename T> void bar1(T f)
  {
    if (f(0))
      foo1();
  }

  template<typename T> void bar2(T);
};

template<typename T> void A::bar2(T f)
{
  A a, b[1], *p;

  while (foo1())
  {
    if (p)
      ++p;
    if (p && foo1())
      bar1(f);
    if (p)
      ++p;
  }

  if (foo1())
    bar1(f);
}

void baz()
{
  A().bar2(foo0);
}

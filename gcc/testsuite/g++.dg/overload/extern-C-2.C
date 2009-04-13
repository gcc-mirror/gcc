// PR c++/39742

void f( int, ...);

struct S
{
};

void
g()
{
  void f( int, ...);

  S t;

  f(1, t);
}

void
f( int i, ...)
{
}

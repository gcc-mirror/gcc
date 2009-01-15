// PR c++/36334

struct X
{
  typedef int* foobar();
  static void foo(foobar&);
};

void X::foo(foobar&)
{
}

struct Y : public X
{
  static foobar bar;
  static void foo();
};

void Y::foo()
{
  X::foo(bar);
}
int* Y::bar()
{
  return 0;
}

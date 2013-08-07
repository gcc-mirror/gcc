// PR c++/46206

class Foo1
{
  int u, v, w, x;
  typedef struct Bar { } Bar;
  virtual void foo(void) {
    struct Bar bar;
  }
};

class Foo2
{
  int u, v, w;
  typedef struct Bar { } Bar;
  Bar bar;
  virtual void foo(void) {
    struct Bar bar;
  }
};

class Foo3
{
  int u, v, w;
  typedef struct Bar { } Bar;
  int Bar;   // { dg-error "conflicts" }
  virtual void foo(void) {
    struct Bar bar;
  }
};

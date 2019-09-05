// PR c++/91644 - ICE with constinit in function template.
// { dg-do compile { target c++11 } }

template <typename T>
static void fn1 ()
{
  static __constinit auto v1 = 0;
  static __constinit int v2 = 0;
}

int nonconst;

template <typename T>
static void fn2 ()
{
  static __constinit auto v1 = nonconst; // { dg-error "does not have a constant initializer|not usable" }
  static __constinit int v2 = nonconst; // { dg-error "does not have a constant initializer|not usable" }
}

template <typename T>
static void fn3 ()
{
  static __constinit T v1 = 0;
  static __constinit T v2 = nonconst; // { dg-error "does not have a constant initializer|not usable" }
}

void
g ()
{
  fn1<int>();
  fn2<int>();
  fn3<int>();
}

// PR c++/98687
// { dg-do compile }

struct S { };

namespace N {
  template <typename T>
  bool operator==(T, int);

  template <typename T>
  void X(T);
}

namespace M {
  template <typename T>
  bool operator==(T, double);
}

template<typename T>
bool fn1 (T t)
{
  using N::operator==;
  return t == 1;
}

template<typename T>
bool fn2 (T t)
{
  // Redeclaration.
  using N::operator==;
  using N::operator==;
  return t == 1;
}

template<typename T>
bool fn3 (T t)
{
  // Need update_local_overload.
  using N::operator==;
  using M::operator==;
  return t == 1;
}

template<typename T>
void fn4 (T)
{
  struct X { };
  using N::X;
  X(1);
}

template<typename T>
void fn5 (T)
{
  int S;
  using ::S;
  struct S s;
}

void
g ()
{
  S s;
  fn1 (s);
  fn2 (s);
  fn3 (s);
  fn4 (s);
  fn5 (s);
}

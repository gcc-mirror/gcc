// PR c++/85587
// { dg-do compile { target c++11 } }

template <int N>
struct S
{
  enum class T
  {
    E, F
  };
  void foo ();
};

template <int N>
void S<N>::foo ()
{
  decltype (T::F) t;
}

void
bar ()
{
  S<0> s;
  s.foo ();
}

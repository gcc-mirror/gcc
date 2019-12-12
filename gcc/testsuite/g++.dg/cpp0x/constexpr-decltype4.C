// PR c++/52269
// { dg-do compile { target c++11 } }

template<typename T>
int f(T x)
{
  return x.get();
}

// O.K. The body of `f' is not required.
decltype(f(0)) a;

template<typename T>
constexpr int g(T x)
{
  return x.get();
}

// Seems to instantiate the body of `g'
// and results in an error.
decltype(g(0)) b;

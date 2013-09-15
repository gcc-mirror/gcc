// { dg-do run { target c++11 } }

int g() { return 0; }
template <class T, class... U>
int g(T t, U... u)
{
  return t + g(u...);
}

template <class... T>
int f1(T... t)
{
  return [t...] {
    return g(t...);
  }();
}

template <class... T>
int f2(T... t)
{
  return [&t...] {
    return g(t...);
  }();
}

template <class... T>
int f3(T... t)
{
  return [=] {
    return g(t...);
  }();
}

template <class... T>
int f4(T... t)
{
  return [&] {
    return g(t...);
  }();
}

#define assert(E) do { if (!(E)) __builtin_abort(); } while(0)
int main()
{
  assert (f1() == 0);
  assert (f2() == 0);
  assert (f3() == 0);
  assert (f4() == 0);
  assert (f1(42) == 42);
  assert (f2(42) == 42);
  assert (f3(42) == 42);
  assert (f4(42) == 42);
  assert (f1(1,2,3) == 6);
  assert (f2(1,2,3) == 6);
  assert (f3(1,2,3) == 6);
  assert (f4(1,2,3) == 6);
}

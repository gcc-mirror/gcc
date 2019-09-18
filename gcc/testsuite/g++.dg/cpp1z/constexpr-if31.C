// PR c++/81676 - bogus -Wunused warnings in constexpr if.
// { dg-do compile { target c++17 } }
// { dg-options "-Wall -Wextra" }

template <typename T> int
f1 (T v)
{
  T x = 0;
  if constexpr(sizeof(T) == sizeof(int))
    return v + x;
  else
    return 0;
}

template <typename T> int
f2 (T v) // { dg-warning "unused parameter .v." }
{
  T x = 0;
  if constexpr(sizeof(T) == sizeof(int))
    return x;
  else
    return 0;
}

template <typename T> int
f3 (T v)
{
  T x = 0; // { dg-warning "unused variable .x." }
  if constexpr(sizeof(T) == sizeof(int))
    return v;
  else
    return 0;
}

template <typename T> int
f4 (T v)
{
  T x = 0;
  if constexpr(sizeof(T) == sizeof(int))
    return 0;
  else
    return v + x;
}

template <typename T> int
f5 (T v) // { dg-warning "unused parameter .v." }
{
  T x = 0;
  if constexpr(sizeof(T) == sizeof(int))
    return 0;
  else
    return x;
}

template <typename T> int
f6 (T v)
{
  T x = 0; // { dg-warning "unused variable .x." }
  if constexpr(sizeof(T) == sizeof(int))
    return 0;
  else
    return v;
}

int main()
{
  f1(0);
  f1('a');
  f2(0);
  f2('a');
  f3(0);
  f3('a');
  f4(0);
  f4('a');
  f5(0);
  f5('a');
  f6(0);
  f6('a');
}

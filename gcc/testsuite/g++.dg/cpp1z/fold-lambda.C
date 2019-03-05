// { dg-do run { target c++17 } }

template <class... T>
auto f() {
  int i = 42;
  return ([i]{ return T(i); }() + ...);
}

int main()
{
  if (f<int,double>() != 84)
    __builtin_abort();
}

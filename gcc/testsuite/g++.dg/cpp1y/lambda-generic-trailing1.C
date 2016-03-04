// { dg-do compile { target c++14 } }

template <class T>
void f()
{
  auto lam = [](auto a)->decltype(++a) { return a; };
}

int main()
{
  f<int>();
}

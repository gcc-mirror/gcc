// PR c++/70735
// { dg-do run { target c++1y } }

template <class T>
static void g()
{
  static int a;
  auto f = [](auto) { return a; };
  if (f(0) != 0)
    __builtin_abort();
  a = 1;
  if (f(0) != 1)
    __builtin_abort();
}

int main()
{
  g<int>();
}

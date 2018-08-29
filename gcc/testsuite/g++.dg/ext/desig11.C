// PR c++/85136
// { dg-options "" }

enum { e };

template<int I> void f()
{
  const int x[] = { [e] = 0 };
  const int y[] = { [I] = 0 };
}

int main()
{
  f<0>();
}

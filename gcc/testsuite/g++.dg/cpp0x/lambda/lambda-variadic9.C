// PR c++/90538
// { dg-do compile { target c++11 } }

template <class... Ts>
void f(Ts... ts)
{
  [=]{
    f(ts...);
    f(ts...);
  }();
}

void g()
{
  f(1);
}

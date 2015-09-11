// PR c++/64834
// { dg-do compile { target c++14 } }

template <typename F>
void e(F f)
{
  f(1);
}

template <int I>
void bar() {
    int x;
    e([&] (const int& y) { (void)sizeof(x); });
    e([&] (const auto& y) { (void)sizeof(x); });
}

void baz() { bar<1>(); }

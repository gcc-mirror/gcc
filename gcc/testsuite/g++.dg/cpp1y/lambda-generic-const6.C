// PR c++/88761
// { dg-do compile { target c++14 } }

template <class T>
void f(T t) { t(1); }

int main()
{
  const unsigned long nf = 10'000'000;

  auto loop = [&](auto)
  {
    auto x = +nf;
    auto y = &nf;
  };

  f(loop);
}

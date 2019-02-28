// PR c++/89522
// { dg-do compile { target c++14 } }

template <typename F>
void foo (F f)
{
  f (1);
}
template <typename T>
void bar (T)
{
  auto f = [&](auto i) { if (f); };  // { dg-error "use of .f. before deduction of .auto." }
  foo (f);
}
int main ()
{
  bar (0);
}

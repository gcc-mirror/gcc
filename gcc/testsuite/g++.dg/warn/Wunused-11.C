// PR c++/20212
// { dg-options "-O2 -Wunused -Wextra" }

template<int> void f(int);
void g(int i)
{
  f<0>(i);
}
template<int> void f(int i __attribute__((unused)) )
{}


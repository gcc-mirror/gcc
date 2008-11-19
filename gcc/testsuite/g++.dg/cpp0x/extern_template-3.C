// PR c++/37256
// { dg-options "-O" }

template <typename T_>
struct B
{
  T_ f();
};

extern template class B<int>;

void f()
{
  B<int> t;
  t.f();
}

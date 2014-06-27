// PR c++/61433
// { dg-do compile { target c++11 } }
// { dg-options "-O -fcompare-debug -fno-inline -fno-ipa-pure-const -fipa-sra" }

template <class T>
struct A
{
  template <class V>
  struct B
  {
    int MEM;
  };
};
struct D {};
struct C: public A<int>::B<D>
{};
template <class T, class U, class V>
auto k(T t, U u, V v) -> decltype (t.U::template B<V>::MEM)
{}
int main()
{
  k( C(), A<int>(), D() );
}

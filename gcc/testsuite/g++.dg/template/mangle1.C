// PR c++/17324
// { dg-do assemble }

template<int, typename T> struct A
{
  template<int I> void foo(const A<I,T>&) {}
};

template<typename> struct B
{
  template<int J> void bar(const A<J,B>&);
  void baz() { A<0,B>().foo(A<0,B>()); }
};

template struct B<void>;
template struct B<int>;

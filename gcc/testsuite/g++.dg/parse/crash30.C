// { dg-do compile }
namespace N { template<int> struct A { operator int() const; }; }
namespace M { template<int> struct A {}; }
namespace P { typedef int I; }

template<typename> void foo()
{
  +typename N::A<0>();  // { dg-bogus "expected" }
}

template<typename> void bar()
{
  +typename M::A<0>;  // { dg-error "expected" }
}

template<typename T> void baz() {
  typename P::I i;  // { dg-bogus "expected" }
}

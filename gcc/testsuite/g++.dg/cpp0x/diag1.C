// { dg-options -std=c++0x }

template <int U>
struct TypeA
{
  typedef int type;
};

template <int N>
struct TypeB
{
  template <int U> typename TypeA<U>::type fn();
};

struct TypeC
{
  TypeB<10> b;
  // This was being printed as:
  // template<int N>
  //   decltype (((TypeC*)this)->
  //             TypeC::b.
  //             template<int U> typename TypeA<U>::type TypeB::fn [with int U = U, int N = 10, typename TypeA<U>::type = TypeA<U>::type]())
  //   TypeC::fn()
  // we don't want to see the template header, return type, or parameter bindings
  // for TypeB::fn.
  template <int N> auto fn() -> decltype(b.fn<N>()); // { dg-bogus "typename|with" }
};

int main()
{
  TypeC().fn<4>(1);		// { dg-error "no match" }
}

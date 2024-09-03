// A version of alias-decl-pr92206-1.C where alias2 is a compound type.
// { dg-require-effective-target c++11 }

template<typename> struct A {};
template<typename T1, typename T2 = typename T1::value> using alias1 = A<T1>;
template<typename T> class B {
  using alias2 = alias1<A<T>>*; // { dg-error {no type named 'value'} }
  A<alias2> a; // { dg-bogus {no type named 'value'} }
};
B<int> b;

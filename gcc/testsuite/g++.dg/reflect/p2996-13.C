// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test from [expr.prim.req.type].

template<typename T, typename T::type = 0> struct S;
template<typename T> using Ref = T&;
template<typename T> concept C = requires {
  typename T::inner;
  // required nested member name
  typename S<T>;
  // required valid (13.3) template-id; fails if T::type does not exist as a type
  // to which 0 can be implicitly converted
  typename Ref<T>;
  // required alias template substitution, fails if T is void
  typename [:T::r1:];
  // fails if T::r1 is not a reflection of a type
  typename [:T::r2:]<int>;
  // fails if T::r2 is not a reflection of a template Z for which Z<int> is a type
};

// { dg-do compile }

struct B { };
union U { };

template<typename T>
  struct underlying_type
  { typedef __underlying_type(T) type; }; // { dg-error "not an enumeration" }

__underlying_type(int) i1; // { dg-error "not an enumeration|invalid" }
__underlying_type(A)   i2; // { dg-error "expected" }
__underlying_type(B)   i3; // { dg-error "not an enumeration|invalid" }
__underlying_type(U)   i4; // { dg-error "not an enumeration|invalid" }

underlying_type<int>::type i5;
underlying_type<A>::type   i6; // { dg-error "not declared|template|expected" }
underlying_type<B>::type   i7;
underlying_type<U>::type   i8;

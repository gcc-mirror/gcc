// { dg-do compile { target c++11 } }

struct A { };
struct B { B(); operator int(); };
struct C {
  C() = default;
  C(const C&);
  C(C&&) = default;
  C& operator=(C&&);
  C& operator= (const C&) = default;
};
struct D { ~D() noexcept(false) {} };

#define SA(X) static_assert((X),#X)

SA(__is_nothrow_constructible(A));
SA(__is_nothrow_constructible(A,A));
SA(!__is_nothrow_constructible(B));
SA(__is_nothrow_constructible(B,B));

SA(!__is_nothrow_constructible(A,B));
SA(!__is_nothrow_constructible(B,A));

SA(__is_nothrow_constructible(C));
SA(__is_nothrow_constructible(C,C));
SA(!__is_nothrow_constructible(C,C&));
SA(__is_nothrow_assignable(C,C&));
SA(!__is_nothrow_assignable(C,C));
SA(!__is_nothrow_assignable(C,C&&));
SA(!__is_nothrow_assignable(void,int));
SA(!__is_nothrow_assignable(const void,int));
SA(!__is_nothrow_assignable(volatile void,int));
SA(!__is_nothrow_assignable(const volatile void,int));

SA(__is_nothrow_constructible(int,int));
SA(__is_nothrow_constructible(int,double));
SA(!__is_nothrow_constructible(int,B));
SA(!__is_nothrow_constructible(void,int));
SA(!__is_nothrow_constructible(const void,int));
SA(!__is_nothrow_constructible(volatile void,int));
SA(!__is_nothrow_constructible(const volatile void,int));
SA(!__is_nothrow_constructible(int, void*));
SA(!__is_nothrow_constructible(int, int*));
SA(!__is_nothrow_constructible(int, const int*));
SA(!__is_nothrow_constructible(int*, void*));
SA(!__is_nothrow_constructible(int*, const int*));

SA(!__is_nothrow_constructible(D));

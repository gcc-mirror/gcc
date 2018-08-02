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
struct D { ~D() {} };

#define SA(X) static_assert((X),#X)

SA(__is_trivially_constructible(A));
SA(__is_trivially_constructible(A,A));
SA(!__is_trivially_constructible(B));
SA(__is_trivially_constructible(B,B));

SA(!__is_trivially_constructible(A,B));
SA(!__is_trivially_constructible(B,A));

SA(__is_trivially_constructible(C));
SA(__is_trivially_constructible(C,C));
SA(!__is_trivially_constructible(C,C&));
SA(__is_trivially_assignable(C,C&));
SA(!__is_trivially_assignable(C,C));
SA(!__is_trivially_assignable(C,C&&));
SA(!__is_trivially_assignable(void,int));
SA(!__is_trivially_assignable(const void,int));
SA(!__is_trivially_assignable(volatile void,int));
SA(!__is_trivially_assignable(const volatile void,int));

SA(__is_trivially_constructible(int,int));
SA(__is_trivially_constructible(int,double));
SA(!__is_trivially_constructible(int,B));
SA(!__is_trivially_constructible(void,int));
SA(!__is_trivially_constructible(const void,int));
SA(!__is_trivially_constructible(volatile void,int));
SA(!__is_trivially_constructible(const volatile void,int));
SA(!__is_trivially_constructible(int, void*));
SA(!__is_trivially_constructible(int, int*));
SA(!__is_trivially_constructible(int, const int*));
SA(!__is_trivially_constructible(int*, void*));
SA(!__is_trivially_constructible(int*, const int*));

SA(!__is_trivially_constructible(D));

SA(__is_trivially_copyable(int));
SA(!__is_trivially_copyable(volatile int));

struct E1 {const int val;};
SA(__is_trivially_copyable(E1));
struct E2 {int& val;};
SA(__is_trivially_copyable(E2));

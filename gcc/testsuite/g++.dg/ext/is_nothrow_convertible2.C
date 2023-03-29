// PR c++/106784
// { dg-do compile { target c++11 } }

#define SA(X) static_assert((X),#X)

struct A { };
struct B { };

struct M {
  operator A();
  operator B() noexcept;
  M(const A&);
  M(const B&) noexcept;
};

SA(!__is_nothrow_convertible(A, M));
SA(!__is_nothrow_convertible(M, A));
SA(__is_nothrow_convertible(B, M));
SA(__is_nothrow_convertible(M, B));

// testcase from P2113
// { dg-do compile { target c++20 } }

template <typename> constexpr bool True = true;
template <typename T> concept C = True<T>;

void f(C auto &, auto &) = delete;
template <C Q> void f(Q &, C auto &);

void g(struct A *ap, struct B *bp) {
  f(*ap, *bp);  // OK: Can use different methods to produce template parameters
}

template <typename T, typename U> struct X {};

template <typename T1, C U1, typename V1>
bool operator==(X<T1, U1>, V1) = delete;

// In P2113 this candidate is reversed.
template <C T2, C U2, C V2>
bool operator==(X<T2, U2>, V2);

void h() {
  X<void *, int>{} == 0; // OK
}

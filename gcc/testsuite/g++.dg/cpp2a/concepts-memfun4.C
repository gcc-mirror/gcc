// PR c++/113191
// { dg-do compile { target c++23 } }

template<typename> struct S;

template<typename T = void>
struct B {
  constexpr int f() const requires true { return 5; }
  constexpr operator int () const requires true { return 5; }
  constexpr int g(this S<T>&&) requires true { return 5; }
  constexpr int h() requires true { return 5; }
};

template<typename = void>
struct S : B<> {
  using B::f;
  using B::g;
  using B::h;
  constexpr int f() const { return 10; }
  constexpr operator int () const { return 10; }
  constexpr int g() { return 10; }
  constexpr int h(this S&&) { return 10; }
};

// implicit object parms match, B::f is more constrained
static_assert(S<>{}.f() == 5);
static_assert(S<>{}.g() == 5);
static_assert(S<>{}.h() == 5);

template <typename = void>
struct C {
  constexpr int f() const { return 15; }
  constexpr operator int () const { return 15; }
};

template <typename = void>
struct S2: B<>, C<> { };

// implicit object parms for conversion functions are all considered to be from
// the class of the object argument
static_assert(S2<>{} == 5);

// ambiguous lookup, so we never actually compare the candidates
// if we did, implicit object parms don't match due to different classes
// so constraints aren't considered and it would still be ambiguous
static_assert(S2<>{}.f() == 5);	// { dg-error "ambiguous" }

template <typename = void>
struct S3 : B<> {
  using B::f;
  constexpr int f() volatile { return 10; }
};

// implicit object parms don't match due to different cv-quals
static_assert(S3<>{}.f() == 5);	// { dg-error "ambiguous" }

template <typename = void>
struct S4 : B<> {
  using B::f;
  constexpr int f() const & { return 10; }
};

// no ref-qual matches any ref-qual
static_assert(S4<>{}.f() == 5);

template <typename = void>
struct C2 {
  constexpr operator int () volatile { return 15; }
};

template <typename = void>
struct S5: B<>, C2<> { };

// implicit object parms don't match due to different cv-quals
static_assert(S5<>{} == 5);	// { dg-error "ambiguous" }

namespace N1 {
  template <class = void> struct B;

  template <class = void>
  struct A {
    constexpr bool operator==(B<>&) { return false; }
  };

  template <class>
  struct B {
    constexpr bool operator==(A<>&) requires true { return true; }
  };

  A<> a;
  B<> b;
  // when comparing the A op== to the reversed B op==, we compare them in
  // reverse order, so they match, and we choose the more constrained.
  static_assert (a == b);
}

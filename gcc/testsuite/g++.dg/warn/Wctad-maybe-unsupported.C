// Test -Wctad-maybe-unsupported.
// { dg-do compile { target c++17 } }
// { dg-options "-Wctad-maybe-unsupported" }

template <typename T> struct Empty { };

template <typename T>
struct A {
  A(T); // generates 'template<class T> A(T)-> A<T>'
  A(T, int); // generates 'template<class T> A(T, int)-> A<T>'
};

// These only succeed because of the implicit guide.  That may be
// undesired.
A a1(42); // { dg-warning "may not intend to support class template argument deduction" }
A a2{42}; // { dg-warning "may not intend to support class template argument deduction" }
A a3 = {42}; // { dg-warning "may not intend to support class template argument deduction" }

template <typename T>
struct B {
  B(T);
  B(T, int);
};
template <typename T> B(T, int) -> B<Empty<T>>;

B b1(42);
B b2{42};
B b3 = {42};

// Motivating examples from Stephan Lavavej's 2018 CppCon talk.
template <class T, class U>
struct Pair {
  T first;
  U second;
  explicit Pair(const T &t, const U &u) {}
};
// deduces to Pair<int, char[12]>
Pair p1(42, "hello world"); // { dg-warning "may not intend to support class template argument deduction" }
Pair p1b{42, "hello world"}; // { dg-warning "may not intend to support class template argument deduction" }

template <class T, class U>
struct Pair2 {
  T first;
  U second;
  explicit Pair2(T t, U u) {}
};
// deduces to Pair2<int, const char*>
Pair2 p2(42, "hello world"); // { dg-warning "may not intend to support class template argument deduction" }
Pair2 p2b{42, "hello world"}; // { dg-warning "may not intend to support class template argument deduction" }

template <class T, class U>
struct Pair3 {
  T first;
  U second;
  explicit Pair3(T const& t, U const& u) {}
};
template<class T1, class T2>
Pair3(T1, T2) -> Pair3<T1, T2>;
 // deduces to Pair3<int, const char*>
Pair3 p3(42, "hello world");
static_assert(__is_same(decltype(p3), Pair3<int, const char*>));

// Test that explicit guides suppress the warning even if they
// aren't used as candidates.
template <typename T>
struct C {
  C(T) { }
};
template <typename T>
explicit C(C<T> const&) -> C<void>;
C<int> c{42};
C c2 = c;
static_assert(__is_same(decltype(c2), C<int>));

// Clang's suppression test.
struct allow_ctad_t {
  allow_ctad_t() = delete;
};

template <typename T>
struct S {
  S(T) {}
};
S(allow_ctad_t) -> S<void>;
S s("abc");
S s2{"abc"};
static_assert(__is_same(decltype(s), S<const char *>));
static_assert(__is_same(decltype(s2), S<const char *>));

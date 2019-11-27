// Test that a non-template deduction guide that doesn't match the alias is
// ignored.
// { dg-do compile { target c++2a } }

template <class T> struct identity { using type = T; };
template <class T> using identity_t = typename identity<T>::type;

template <class T, class U> struct C {
  C(T, U);			// #1
};

C(char*, char*) -> C<int,int>;  // #3

template<class V>
using A = C<V *, V *>;

char c;
A a4 (&c, &c);			// ignores #3 because C<int,int> is not an A<V>

static_assert (__is_same_as(decltype(a4),A<char>));

C c2 (&c, &c);			// { dg-error "conversion" } deduces with #3

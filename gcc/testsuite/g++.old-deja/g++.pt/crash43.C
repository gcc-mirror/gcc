// Build don't link:
// Origin: Mark Mitchell <mark@codesourcery.com>

template <int T>
struct S {
  struct X {};
  struct Y {};

  template <int U>
  friend struct S<U>::X; // ERROR - typename as friend

  template <int U>
  friend typename S<U>::Y; // ERROR - typename as friend
};

struct T {
  template <int T>
  friend struct S<T>::X; // ERROR - typename as friend
};

struct U {
  template <int T>
  friend typename S<T>::X; // ERROR - typename as friend
};

// Build don't link:
// Origin: Mark Mitchell <mark@codesourcery.com>

template <typename T>
struct S {
  enum E { e = 5 };
  static int i[e];
};

template <typename T>
int S<T>::i[S<T>::e];

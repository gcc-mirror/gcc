// Build don't link:
// Origin: Mark Mitchell <mark@codesourcery.com>

template <int I>
struct S {
  struct T* x;
};

template struct S<2>;

T* t;

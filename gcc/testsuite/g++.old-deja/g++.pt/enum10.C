// Build don't link:

template <class T> 
struct S {
  enum E { a = (int) T::b };
};

struct S2 {
  enum E2 { b };
};

template class S<S2>;

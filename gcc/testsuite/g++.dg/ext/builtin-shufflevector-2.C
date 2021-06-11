// { dg-do compile }
// { dg-additional-options "-Wno-psabi" }

typedef double v2df __attribute__((vector_size(2 * sizeof (double))));

template<typename T, typename U, int N, int M>
struct Shuffle {
  void f(T t, U u, v2df a, v2df b) {
    (void)__builtin_shufflevector(t, u, N, M); // { dg-error "invalid" }
  }
};

template struct Shuffle<v2df, v2df, 4, 3>;

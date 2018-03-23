// { dg-do compile }

struct K {
  struct L {
    static double j;
  };
};

template <typename T>
struct M {
  struct N {
    static int i;
  };
};

template <typename T>
struct O {
  typedef M<T> P;
  typedef K Q;
};

template <typename T>
int O<T>::P::N::i = 42; // This is obfuscated, but apparently ok.

template <typename T>
double O<T>::Q::L::j = 42.0; // { dg-error "non-template" }

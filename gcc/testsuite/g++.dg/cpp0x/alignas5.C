// PR c++/58109  - alignas() fails to compile with constant expression
// { dg-do compile }

template <typename T>
struct Base {
  static const int Align = sizeof (T);
};

// Never instantiated.
template <typename T>
struct Derived: Base<T>
{
#if __cplusplus >= 201102L
  // This is the meat of the (simplified) regression test for c++/58109.
  using B = Base<T>;
  using B::Align;

  alignas (Align) char a [1];
  alignas (Align) T b [1];
#else
  // Fake the test for C++ 98.
#  define Align Base<T>::Align
#endif

  char __attribute__ ((aligned (Align))) c [1];
  T __attribute__ ((aligned (Align))) d [1];
};

// Instantiated to verify that the code is accepted even when instantiated.
template <typename T>
struct InstDerived: Base<T>
{
#if __cplusplus >= 201102L
  using B = Base<T>;
  using B::Align;

  alignas (Align) char a [1];
  alignas (Align) T b [1];
#endif

  char __attribute__ ((aligned (Align))) c [1];
  T __attribute__ ((aligned (Align))) d [1];
};

InstDerived<int> dx;

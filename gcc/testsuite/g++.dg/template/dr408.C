// DR 408
// { dg-do link }

// Test that a size given in the out-of-class definition isn't used until
// instantiation time.
template<typename T>
struct X
{
  static char s[];
  int c;
};

template<typename T>
char X<T>::s[sizeof(X<T>)];

#define sassert(EXP) int ar[(EXP)?1:-1]
sassert(sizeof (X<char>::s) == sizeof (int));

// Test that a specialization can have a different size.

template <int> void g();
template <> void g<2>() { }

template <typename T>
struct S {
  static int i[];
  void f();
};

template <typename T>
int S<T>::i[] = { 1 };

template <typename T>
void S<T>::f() {
  g<sizeof (i) / sizeof (int)>();
}

template <>
int S<int>::i[] = { 1, 2 };

int main()
{
  S<int> s;
  s.f();
}

// PR c++/120185

struct A {
  A(...);
};

template <class T> void f(A, T) { }

int main()
{
  f(42, 24);
}

// Build don't run:
// Special g++ Options: -Wno-deprecated

struct B {
  typedef int I;
};

template <class T>
struct D1 : public B {
};

template <class T>
struct D2 : public D1<T> {
  I i;  // WARNING - implicit typename
};

template <>
struct D1<int> {
  typedef double I;
};

template <class T>
void f(T);
template <>
void f(double) {}

int main()
{
  D2<int> d2i;
  f(d2i.i);
}

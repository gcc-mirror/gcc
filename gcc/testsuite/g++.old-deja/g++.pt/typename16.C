// { dg-do link  }
// { dg-options "-Wno-deprecated" }

struct B {
  typedef int I;
};

template <class T>
struct D1 : public B {
};

template <class T>
struct D2 : public D1<T> {
  I i;  // { dg-error "" } not a type
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
  f(d2i.i); // { dg-error "" } no member i
}

// { dg-do assemble  }

template <class T>
struct S1 {};

struct S2
{
  template <class T>
  operator S1<T>*();
};

struct D: public S1<int> {
};

void f()
{
  S2 s;
  (D*) s; // { dg-error "" } cannot convert
}

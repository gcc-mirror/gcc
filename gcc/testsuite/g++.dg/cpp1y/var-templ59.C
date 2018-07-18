// PR c++/71569
// { dg-do compile { target c++14 } }

template <class T>
struct A {
  template <class U>
  static U u;
};

int main()
{
  decltype(A<int>::u) a;	// { dg-error "missing template arguments" }
  return a;
}

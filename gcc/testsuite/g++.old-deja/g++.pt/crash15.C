// { dg-do assemble  }

template <class T>
template <class U>
struct A { // { dg-error "" } too many template parameter lists
public:
  A() {}

  A(const A<T>& b) {}
};

// Build don't link:

template <class T>
template <class U>
struct A { // ERROR - too many template parameter lists
public:
  A() {}

  A(const A<T>& b) {}
};

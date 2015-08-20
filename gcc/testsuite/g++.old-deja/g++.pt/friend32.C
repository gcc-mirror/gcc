// { dg-do assemble  }

template <class T, class U>
struct S {
  template <class X, class Y, class Z>
  friend X f(X, Y, Z);
};

template class S<int, double>;
template char f(char, long, short); // { dg-error "f" }
template char* f(char*, long*, short*); // { dg-error "f" }

template <class X, class Y, class Z>
X f(X x, Y, Z) {
  return x;
}

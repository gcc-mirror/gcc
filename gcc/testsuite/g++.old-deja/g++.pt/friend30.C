// { dg-do assemble  }

template <class T, class U>
struct S {
  template <class X, class Y, class Z>
  friend X f(X, Y, Z);
};

template <class X, class Y, class Z>
X f(X x, Y, Z) {
  return x;
}

template char f(char, long, short);
template char* f(char*, long*, short*);
template class S<int, double>;
template class S<void*, double>;
template double* f(double*, long*, short*);

// { dg-do link }
// { dg-options "-fno-implicit-templates" }

template <class T> struct C {
  ~C();
};
template <class T> C<T>::~C() {}

struct X {
  C<X> *p;
  ~X() { delete p; }
};

template class C<X>;
C<X> x;

int main () {}

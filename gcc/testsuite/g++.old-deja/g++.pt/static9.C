// Build don't link:
// Origin: Mark Mitchell <mark@codesourcery.com>

template <class T>
struct S {
  int i;
};

template <class T>
struct X {
  static S<T> s[];
};

template <class T>
S<T> X<T>::s[] = {
  { 3 } 
};

struct Z {};

void f(S<Z>* s);

void g()
{
  f (X<Z>::s);
}


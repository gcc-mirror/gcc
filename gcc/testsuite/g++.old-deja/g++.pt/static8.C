// { dg-do run  }
// Origin: Mark Mitchell <mark@codesourcery.com>

int i;

template <class T>
struct S {
  S() { ++i; }

  virtual void g() {}
  virtual void f();

  static S s;
};

template <class T>
void S<T>::f() {
  s.f();
}

S<int> si;

template <class T>
S<T> S<T>::s;

int main ()
{
  si.g();
  if (i != 2)
    return 1;
  else
    return 0;
}

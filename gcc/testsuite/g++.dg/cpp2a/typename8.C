// P0634R3
// { dg-do compile { target c++2a } }

template<typename T>
struct S {
  static int foo ();
};

template<typename T>
int
f ()
{
  return S<T>::foo();
}

void
test ()
{
  f<int>();
}

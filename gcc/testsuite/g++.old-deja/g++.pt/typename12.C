// Special g++ Options:
// execution test - XFAIL *-*-*
// excess errors test - XFAIL *-*-*

int i = 0;

template <class T>
struct S {
  struct X {};
};

template <class T>
void f(T)
{
  S<T>::X();
}

template <>
struct S<int> {
  static void X() { i = 1; }
};

int main()
{
  f(3);
  if (i != 1)
    return 1;
  else 
    return 0;
}


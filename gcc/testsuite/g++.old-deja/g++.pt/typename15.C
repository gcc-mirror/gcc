// { dg-do assemble  }
// { dg-options "" }

template <class T, bool B> 
struct R {
  struct X {};
};

template <class T, bool B = false>
struct S : public R <T, B> {
};

template <class T> void f() 
{
  typename S<T>::X();
}

template void f<int>();

// Build don't link:
// Special g++ Options:

template <class T, bool B> 
struct R {
  struct X {};
};

template <class T, bool B = false>
struct S : public R <T, B> {
};

template <class T> void f() 
{
  S<T>::X();
}

template void f<int>();

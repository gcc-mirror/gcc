// Special g++ Options:
// Origin: Mark Mitchell <mark@codesourcery.com>

template <class T>
struct O {
  typedef char X;
};

template <class T>
struct S {
  typedef double X;

  template <class U>
  struct I : public O<U> {
    static X x; // WARNING - lookup finds S<T>::X
  };
};

template <class T>
template <class U>
typename S<T>::X S<T>::I<U>::x;

int main()
{
  return sizeof (S<int>::I<double>::x) == 1;
}

// { dg-do run  }
// { dg-options "" }
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
    static X x;
  };
};

template <class T>
template <class U>
typename S<T>::X S<T>::I<U>::x;

int main()
{
  return sizeof (S<int>::I<double>::x) == 1;
}

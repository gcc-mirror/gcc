// { dg-do link  }

template<class T>
class foo {
  T deft;

  template<class U> int priv (U u, T t) { return u - t; }
public:
  foo (T t) : deft (t) {}

  template<class U> int pub (U u) {
    int (foo::*fn) (U, T);
    fn = &foo<T>::template priv<U>;
    return (this->*fn) (u, deft);
  }
};

int
main ()
{
  foo<long> fff (5);
  return fff.pub (3);
}

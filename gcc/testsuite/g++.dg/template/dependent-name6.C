// PR c++/26261
// { dg-final { scan-assembler "_ZN1YIiE1fIiEE1XILi1EEv" } }

template <int dim> class X {};

template <class T> struct Y {
  static const unsigned int dim = 1;
  template <class U> X<Y<T>::dim> f();
};

template <class T> template <class U>
X<Y<T>::dim> Y<T>::f() { return X<dim>(); }

int main()
{
  Y<int>().f<int>();
}

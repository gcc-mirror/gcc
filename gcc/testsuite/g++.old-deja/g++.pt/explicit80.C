// { dg-do assemble  }
// Bug: We were complaining about explicit instantiation of A<T>::B.

template <class T>
struct A
{
 public:
  ~A() { }

  class B;
};

template <> class A<int>::B { };

template class A<int>;
template class A<double>;

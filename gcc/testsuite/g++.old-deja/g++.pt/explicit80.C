// Bug: We were complaining about explicit instantiation of A<T>::B.
// Build don't link:

template <class T>
struct A
{
 public:
  ~A() { };

  class B;
};

class A<int>::B { };

template class A<int>;
template class A<double>;

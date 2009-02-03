// PR37314 rejects-valid, from w.doeringer
template <typename T>
struct A {
  typedef __PTRDIFF_TYPE__ difference_type;
  struct B {
    typedef typename A<T>::difference_type difference_type;
    difference_type operator-(B const&) const; 
    T t;
  };
};
// 

template <typename T>
typename A<T>::B::difference_type A<T>::B::operator-(B const&) const {
  return -1;
} 

//
int main() {
  A<int>::B i;
  ++i.t;
  return 0;
}



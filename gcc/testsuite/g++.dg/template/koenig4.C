// PR c++/13978

namespace ns {
  template <class TP> void func1(TP* t);
  struct A {};
}
 
template < class TP >
void func2() {
  func1( new ns::A() );
}


// PR c++/86476 - noexcept-specifier is a complete-class context
// { dg-do compile { target c++11 } }

void fn1(void());
template <typename> class A {
  void _M_local_data();
  A() noexcept(_M_local_data);
};

class B {
  void _S_initialize();
  static void _S_initialize_once();
};
void B::_S_initialize() { fn1(_S_initialize_once); }

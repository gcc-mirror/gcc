// { dg-do assemble  }
class foo {
public:
  void apply(foo *(foo::*memptr)()) {
    this->*memptr();		// { dg-error "" } wrong
  }
};

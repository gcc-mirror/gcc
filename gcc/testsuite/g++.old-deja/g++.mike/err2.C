class foo {
public:
  void apply(foo *(foo::*memptr)()) {
    this->*memptr();		// ERROR - wrong
  }
};

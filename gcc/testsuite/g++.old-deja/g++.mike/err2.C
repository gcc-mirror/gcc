class foo {
public:
  apply(foo *(foo::*memptr)()) {
    this->*memptr();		// ERROR - wrong
  }
};

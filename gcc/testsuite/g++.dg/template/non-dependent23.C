// PR c++/105637

struct Base {
  void foo();                // #1
  void foo() const;          // #2
  void foo() volatile;       // #3
  void foo() const volatile; // #4
};

template<class T>
struct TopClass : T {
  void failsToCompile() const {
    Base::foo(); // should select #2, not #1
  }

  void failsToCompile() volatile {
    Base::foo();  // should select #3, not #1
  }

  void failsToCompile() const volatile {
    Base::foo();  // should select #4, not #1
  }
};

template struct TopClass<Base>;

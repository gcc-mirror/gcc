// PR c++/105637
// { dg-do compile { target c++11 } }

struct Base {
  void foo();                // #1
  void foo() const = delete; // #2
};

template<class T>
struct TopClass : T {
  void failsToCompile() {
    [this] { Base::foo(); }(); // should select #2, not #1
  }

  void failsToCompile() const {
    [this] { Base::foo(); }(); // { dg-error "deleted" }
  }
};

template struct TopClass<Base>;

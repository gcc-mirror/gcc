// { dg-do compile }

// Origin: Mike Reed <mike.reed@amadron.com>

// PR c++/11174: Access checking of pointer-to-member function

class A {
protected:
  void foo() {}			// { dg-message "protected" }
public:
  A();
};

class B : public A {
  void bar() {
    A a;
    void (A::*pmf)() = &A::foo;	// { dg-error "this context" }
    (a.*pmf)();
  }
};

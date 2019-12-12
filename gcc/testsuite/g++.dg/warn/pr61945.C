// PR c++/61945
// { dg-do compile }
// { dg-options "-Woverloaded-virtual" }

class A {
  virtual int foo ();	// { dg-warning "was hidden" }
};
class B : A {
  template <typename>
  void foo ();		// { dg-message "by .B::foo\\(\\)." }
};

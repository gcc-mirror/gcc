// PR sanitizer/66977
// { dg-do compile }
// { dg-options "-fsanitize=shift -Wmaybe-uninitialized -O" }

class Foo {

private:

  int a_;

public:

  Foo (int a) : a_(a) {};

  inline int get_a () { return a_; };
};

int bar (int (Foo::*get)()) {
  Foo *A = new Foo(1);
  int result = (A->*get)();
  delete (A);
  return result;
}

int main () {
  return bar (&Foo::get_a);
}

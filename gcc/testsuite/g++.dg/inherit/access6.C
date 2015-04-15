// PR c++/28588

class Foo { 
  static void f(); // { dg-message "private" }
  static void f(int);
  static void g(); // { dg-message "private" }
};

void h()
{
  Foo foo;
  void (*f)();
  f = foo.f; // { dg-error "context" }
  f = foo.g; // { dg-error "context" }
}

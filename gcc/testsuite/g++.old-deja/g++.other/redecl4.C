// { dg-do assemble  }
int main() {
  struct A {
    void f();			// { dg-error "with" } already declared
    void f();			// { dg-error "overloaded" } already declared
  };
}

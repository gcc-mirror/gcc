// { dg-do assemble  }
int main() {
  struct A {
    void f();			// { dg-message "previous" } already declared
    void f();			// { dg-error "overloaded" } already declared
  };
}

// { dg-do assemble  }
int main() {
  struct A {
    void f();
    void f();			// { dg-error "" } already declared
  };
}

int main() {
  struct A {
    void f();
    void f();			// ERROR - already declared
  };
}

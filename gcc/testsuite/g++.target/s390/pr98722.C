// { dg-do compile }
// { dg-options "-Og -fno-tree-fre -fno-split-wide-types" }
struct B {
  virtual void Method();
};
typedef void (B::*fn_type_a)();

int main() {
  fn_type_a f(&B::Method);
  B b;
  (b.*f)();
}

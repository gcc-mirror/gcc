// PR c++/114868
// { dg-additional-options "-fmodules-ts" }
import M;

int main() {
  bar::a();
  foo::a();  // { dg-error "not been declared" }
}

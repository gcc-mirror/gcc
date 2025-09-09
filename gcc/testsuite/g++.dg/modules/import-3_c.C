// PR c++/99682
// { dg-additional-options "-fmodules" }

module foo;
import bar;  // not an interface dependency
import bar;  // double import is not an error either

void test() {
  foo();
  bar();
}

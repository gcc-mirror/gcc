// PR c++/122279
// { dg-additional-options "-fmodules" }

module M;
void test2() {
  qux::go();
}

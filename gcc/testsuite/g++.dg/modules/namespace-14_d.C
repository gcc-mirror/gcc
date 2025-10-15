// PR c++/122279
// { dg-additional-options "-fmodules" }
// { dg-module-cmi M:other_part }

module M:other_part;
import :part;

void test3() {
  bar::go();
}

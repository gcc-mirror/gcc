// PR c++/122279
// { dg-additional-options "-fmodules" }
// { dg-module-cmi M }

export module M;
import :part;
namespace qux {
  using namespace bar;
}
void test1() {
  bar::go();
}

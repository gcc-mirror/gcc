// PR c++/122279
// { dg-additional-options "-fmodules" }
// { dg-module-cmi M:part }

module M:part;
namespace foo {
  void go();
}
namespace bar {
  using namespace foo;
}

// PR c++/104040
// { dg-additional-options "-fmodules-ts" }
// { dg-module-cmi test }

export module test;

export template <typename T>
struct test {
  ~test() {}
};

test<bool> use() {
  return {};
}

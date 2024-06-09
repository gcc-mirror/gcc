// { dg-additional-options "-fmodules-ts" }

module mod;

// Test that we can access (and link) to declarations from the interface
void test_from_impl_unit() {
  only_used_in_impl().f();
}

// PR c++/115801
// { dg-additional-options "-fmodules-ts" }

import test;

int main() {
  GMF<int> gmf;
  Attached<int> attached;

  int a = test_gmf<double>();
  int b = test_attached<double>();

  GMF_Hidden<int> gmf_hidden;  // { dg-error "not declared" }
  Attached_Hidden<int> attached_hidden;  // { dg-error "not declared" }
}

// { dg-prune-output "expected primary-expression" }

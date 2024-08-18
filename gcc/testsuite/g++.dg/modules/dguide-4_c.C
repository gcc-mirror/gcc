// PR c++/116403
// { dg-additional-options "-fmodules-ts" }

import B;

int main() {
  GMF gmf(123);
  GMF<int> test_gmf = gmf;

  Attached attached(456);
  Attached<int> test_attached = attached;
}

GMF(int) -> GMF<double>;  // { dg-error "ambiguating" }
Attached(int) -> Attached<double>;  // { dg-error "ambiguating" }

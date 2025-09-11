// PR c++/121705
// { dg-additional-options "-fmodules" }

import M;
int main() {
  test_op(E{}, 0);

  test_using(E{}, 0);  // { dg-message "here" }
  // { dg-error "no match for 'operator<'" "" { target *-*-* } 0 }

  test_unused(E{});  // { dg-message "here" }
  // { dg-error "'unused' was not declared" "" { target *-*-* } 0 }
}

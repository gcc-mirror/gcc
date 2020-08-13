// PR c++/93529
// P1009: Array size deduction in new-expressions
// { dg-do compile { target c++11 } }

void
fn ()
{
  new int[][3]{ { 1, 2, 3 } };
  new int[][]{ { 1, 2, 3 } }; // { dg-error "expected primary-expression" }
}

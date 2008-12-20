/* PR 36921: comparison operator can be overloaded. Do not emit
   warnings in such case.
 { dg-do compile }
 { dg-options "-Wparentheses" }
*/
struct A {};
A operator<(A, A) { return A(); }
A operator>(A, A) { return A(); }
A operator<=(A, A) { return A(); }
A operator>=(A, A) { return A(); }
A operator==(A, A) { return A(); }
A operator!=(A, A) { return A(); }

int main() {
  A() < A() < A(); // should not emit warning
  1 < 2 < 3; // { dg-warning "mathematical meaning" "parentheses" }
  A() > A() > A(); // should not emit warning
  1 > 2 > 3; // { dg-warning "mathematical meaning" "parentheses" }
  A() <= A() <= A(); // should not emit warning
  1 <= 2 <= 3; // { dg-warning "mathematical meaning" "parentheses" }
  A() >= A() >= A(); // should not emit warning
  1 >= 2 >= 3; // { dg-warning "mathematical meaning" "parentheses" }

  A() == A() < A (); // { dg-warning "suggest parentheses" "parentheses" }
  A() < A() != A (); // { dg-warning "suggest parentheses" "parentheses" }
  return 0;
}

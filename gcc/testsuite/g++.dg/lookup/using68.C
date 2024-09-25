// { dg-do compile }

struct S {}; // { dg-message "previous" }
void foo() {
  using ::S;
  struct S {};  // { dg-error "redefinition" }
}

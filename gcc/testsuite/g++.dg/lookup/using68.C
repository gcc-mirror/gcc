// { dg-do compile }

struct S {}; // { dg-message "here" }
void foo() {
  using ::S; // { dg-message "previous" }
  struct S {};  // { dg-error "conflicts" }
}

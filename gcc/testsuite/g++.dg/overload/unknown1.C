// { dg-do compile }

void foo(void);
int foo(int);
template <typename T> void bar(T f); // { dg-message "note" }

void baz() {
  bar(foo); // { dg-error "<unresolved overloaded function type>" }
  // { dg-message "candidate" "candidate note" { target *-*-* } 8 }
}

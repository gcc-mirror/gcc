// { dg-do compile }
void foo() __attribute__((unavailable));
void bar () {
  foo (); // { dg-bogus "is unavailable.*is unavailable" }
  // { dg-error "is unavailable" "" { target *-*-* } .-1 }
}

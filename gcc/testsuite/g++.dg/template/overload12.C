// { dg-do compile }

struct S {int x; int y;};
template<typename T>
int foo(T a, T b) {return a + b;} // { dg-message "template" }
template<typename T, typename T2>
int foo(T a, T2& b, T2 c) {return a + b;}  // { dg-message "template" }
int foo(char*, S&); // { dg-message "foo" }
// { dg-message "candidate expects 2 arguments, 3 provided" "arity" { target *-*-* } 8 }

int foo2(int x)
{
  S s={1,2};
  char c;
  foo(c, 2, c); // { dg-error "no matching function" }
  // { dg-message "(candidate|deduced conflicting types for)" "candidate note" { target *-*-* } 15 }
}

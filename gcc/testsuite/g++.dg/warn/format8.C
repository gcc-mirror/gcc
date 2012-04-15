// PR c++/52818
// { dg-options "-pedantic-errors -Wformat" }

extern "C" int printf (const char *, ...);
void f() {
  printf("%lf", 0.0);		// { dg-warning "%lf" "" { target c++98 } }
}

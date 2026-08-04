// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

constexpr auto r = ^^int;

void f () { (void) r; }		      // { dg-error "consteval-only" }
void g (bool c) { if (c) (void) r; }  // { dg-error "consteval-only" }
auto h () { return r; }		      // { dg-error "consteval-only" }

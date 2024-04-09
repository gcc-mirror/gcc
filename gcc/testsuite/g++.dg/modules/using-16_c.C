// { dg-additional-options "-fmodules-ts" }
import M;

int main() {
  // this should be hidden and fail
  foo::f(foo::S{});  // { dg-error "cannot convert" }

  // but these should be legal
  foo::f(10);
  f(foo::S{});
}

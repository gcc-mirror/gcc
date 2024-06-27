// { dg-additional-options "-fmodules-ts" }

import M;
namespace foo {}

int main() {
  struct bar::S a = bar::S;
  bar::X b = bar::S;

  struct foo::S c;  // { dg-error "does not name a type" }
  auto d = foo::S;  // { dg-error "not a member" }
}

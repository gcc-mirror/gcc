// PR c++/120499
// { dg-module-do link }
// { dg-additional-options "-fmodules -fdump-lang-module" }

import B;

int main() {
  createColl();
}

// And we should use the definition of _Vector_impl::~_Vector_impl now.
// In this case we got the seeded import from A of the declaration
// so let's just ensure we stream the definition.
// { dg-final { scan-lang-dump {Reading function definition '::vector@A:1<int>::_Vector_impl@A:1<int>::__dt @A:1'} module } }

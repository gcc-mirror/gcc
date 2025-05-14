// PR c++/119564
// { dg-additional-options "-fmodules -Wtemplate-names-tu-local" }
// { dg-module-cmi M }

export module M;
static void foo() {};  // { dg-message "declared" }
template <typename> void bar() { foo(); }  // { dg-warning "TU-local" }

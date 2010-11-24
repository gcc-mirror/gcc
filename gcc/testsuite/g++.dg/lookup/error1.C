// PR c++/17609
// Origin: <papadopo@shfj.cea.fr>
// { dg-do compile }

namespace N { int i; }
void foo() { i; }   // { dg-error "not declared" }
  // { dg-message "note" "suggested alternative" { target *-*-* } 6 }

using namespace N;
void bar() { i; }

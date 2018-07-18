// PR c++/17609
// Origin: <papadopo@shfj.cea.fr>
// { dg-do compile }

namespace N { int i; }		// { dg-message "N::i" }
void foo() { i; }   // { dg-error "not declared" }
  // { dg-message "suggested alternative" "suggested alternative" { target *-*-* } .-1 }

using namespace N;
void bar() { i; }

// PR c++/17609
// Origin: <papadopo@shfj.cea.fr>
// { dg-do compile }

namespace N { int i; }
void foo() { i; }   // { dg-error "not declared" }

using namespace N;
void bar() { i; }

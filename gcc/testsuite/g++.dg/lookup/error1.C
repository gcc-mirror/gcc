// PR c++/17609
// Origin: <papadopo@shfj.cea.fr>
// { dg-do compile }

namespace N { int i; }		// { dg-message "N::i" }
void foo() { i; }   // { dg-error "'i' was not declared in this scope; did you mean 'N::i'\\?" }

using namespace N;
void bar() { i; }

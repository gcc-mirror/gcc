// PR c++/118920
// { dg-additional-options "-fmodules" }
// { dg-module-cmi M }

module;
#include "tpl-friend-17.h"
export module M;
unique_ptr<int> s;
export template <typename> void foo() { shared_ptr<int> u; }

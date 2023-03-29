// PR c++/100134
// { dg-additional-options -fmodules-ts }
// { dg-module-cmi pr100134 }
export module pr100134;

import "tpl-friend-8_a.H";

export std::A<int> a;

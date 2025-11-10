// PR c++/100134
// { dg-additional-options "-fmodules -Wno-global-module" }
// { dg-module-cmi pr100134 }
module;
namespace outer::inner {
  inline namespace more_inner {}
}
export module pr100134;

import "tpl-friend-8_a.H";

export std::A<int> a;
export outer::inner::B<int> b;

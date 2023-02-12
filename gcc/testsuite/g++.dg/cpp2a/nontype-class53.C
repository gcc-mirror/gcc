// PR c++/103346
// { dg-do compile { target c++20 } }

struct Item {};

template<class T, T... ts>
struct Sequence { };

template<Item... items>
using ItemSequence = Sequence<Item, items...>;

template<Item... items>
constexpr auto f() {
  constexpr auto l = [](Item item) { return item; };
  return ItemSequence<l(items)...>{};
}

using ty0 = decltype(f<>());
using ty0 = ItemSequence<>;

using ty1 = decltype(f<{}>());
using ty1 = ItemSequence<{}>;

using ty3 = decltype(f<{}, {}, {}>());
using ty3 = ItemSequence<{}, {}, {}>;

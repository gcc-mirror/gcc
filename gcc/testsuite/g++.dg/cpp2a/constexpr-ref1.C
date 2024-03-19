// P2280R4 - Using unknown pointers and references in constant expressions
// PR c++/106650
// { dg-do compile { target c++20 } }

#include <typeinfo>

using size_t = decltype(sizeof(42));

template <typename T, size_t N>
constexpr size_t array_size(T (&)[N]) {
  return N;
}

void use_array(int const (&gold_medal_mel)[2]) {
  constexpr auto gold = array_size(gold_medal_mel);     // OK
}

constexpr auto olympic_mile() {
  const int ledecky = 1500;
  return []{ return ledecky; };
}
static_assert(olympic_mile()() == 1500);                // OK

struct Swim {
  constexpr int phelps() { return 28; }
  virtual constexpr int lochte() { return 12; }
  int coughlin = 12;
};

constexpr int how_many(Swim& swam) {
  Swim* p = &swam;
  return (p + 1 - 1)->phelps();
}

void splash(Swim& swam) {
  static_assert(swam.phelps() == 28);           // OK
  static_assert((&swam)->phelps() == 28);       // OK

  Swim* pswam = &swam;
  static_assert(pswam->phelps() == 28);         // { dg-error "non-constant|not usable" }

  static_assert(how_many(swam) == 28);          // OK
  static_assert(Swim().lochte() == 12);         // OK

  static_assert(swam.lochte() == 12);           // { dg-error "non-constant|not a constant" }

  static_assert(swam.coughlin == 12);           // { dg-error "non-constant|not a constant" }
}

extern Swim dc;
extern Swim& trident;

constexpr auto& sandeno   = typeid(dc);         // OK, can only be typeid(Swim)
constexpr auto& gallagher = typeid(trident);    // { dg-error "not a constant" }

// PR c++/86378
// { dg-do compile { target c++11 } }

struct Pepper {};
struct Apple { Apple(int) {} };

struct Combination : Apple, Pepper
{
  Combination(Pepper p, Apple a)
    : Apple(a), Pepper(p)
  {}
};

struct MyCombination
{
  using Spice = Pepper;
  using Fruit = Apple;

  Combination combination;

  template<typename T>
  constexpr MyCombination(T&& t)
  noexcept(noexcept(Combination(Spice(), Fruit(t))))
    : combination(Spice(), Fruit(t))
  {}
};

MyCombination obj(Apple(4));

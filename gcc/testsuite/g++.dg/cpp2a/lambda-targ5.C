// PR c++/115378
// { dg-do compile { target c++20 } }

struct tt {};

template<class Slot, auto Tag = []{}>
constexpr auto __counter = 1;

template <class Child, int Counter>
using _as_base = tt;

template <class... Envs>
struct env : _as_base<Envs, __counter<int>>... {};

env<int> t;

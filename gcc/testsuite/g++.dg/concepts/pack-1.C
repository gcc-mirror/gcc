// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

// distilled from <concepts>, via header units

template<typename _ArgTypes>
struct is_invocable;

template<typename... _Args>
concept invocable = is_invocable<_Args...>::value;

template<typename _Is>
requires invocable<_Is>
class BUG;

template<typename _Is>
requires invocable<_Is>
class BUG {}; // { dg-bogus "different constraints" }

template<int> struct is_invocable_NT;

template<int... Ints>
concept invocable_NT = is_invocable_NT<Ints...>::value;

template<int _Is>
requires invocable_NT<_Is>
class BUG_NT;

template<int _Is>
requires invocable_NT<_Is>
class BUG_NT {};

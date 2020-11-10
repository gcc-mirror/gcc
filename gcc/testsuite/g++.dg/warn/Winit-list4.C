// PR c++/97632
// { dg-do compile { target c++20 } }
// Test we don't warn in an unevaluated operand.

#include <initializer_list>

template<typename _Tp>
concept default_initializable
  = requires
    {
      _Tp{};
      (void) ::new _Tp; // { dg-bogus "does not extend the lifetime" }
    };

static_assert(default_initializable<std::initializer_list<int>>);

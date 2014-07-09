// PR c++/57466
// { dg-do compile { target c++11 } }

template<typename T>
  constexpr bool
  is_pointer(const T*)
  { return true; }

template<typename T>
  constexpr bool
  is_pointer(const T&)
  { return false; }

using F = void();

constexpr F* f = nullptr;

static_assert( is_pointer(f), "function pointer is a pointer" );

// { dg-do compile { target c++20 } }
// { dg-additional-options -fmodules }

export module M;

export
template <class T>
inline void f()
{
  []<int M>() -> int {
    if constexpr (M > 0) { return true; }
    else { return false; }
  };
}

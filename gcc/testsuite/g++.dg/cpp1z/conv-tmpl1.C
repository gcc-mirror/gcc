// PR c++/91465 - ICE with template codes in check_narrowing.
// { dg-do compile { target c++17 } }

enum class E { Z };

template <typename F>
void foo(F)
{
  E{char(0)};
}

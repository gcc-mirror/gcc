// PR c++/54207
// { dg-do compile }
// { dg-options "-std=c++11" }

typedef bool B;
constexpr B foo () { return true; }

void
bar () noexcept (foo ())
{
}

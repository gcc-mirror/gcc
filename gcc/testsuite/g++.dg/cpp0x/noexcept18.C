// PR c++/54207
// { dg-do compile { target c++11 } }

typedef bool B;
constexpr B foo () { return true; }

void
bar () noexcept (foo ())
{
}

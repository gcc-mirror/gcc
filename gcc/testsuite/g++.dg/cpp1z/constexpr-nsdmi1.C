// PR c++/126215
// { dg-do compile { target c++17 } }

struct bug{ bug *ptr = this; };

struct base0 { bug a; };
struct base1 { bug b; };
struct tuple_: base0, base1 {};
struct tuple : tuple_ {};

constexpr tuple gen() { return {}; }
constexpr tuple names = gen();
int main() { bug *x = names.b.ptr; }

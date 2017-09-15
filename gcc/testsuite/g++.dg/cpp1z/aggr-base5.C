// { dg-options "-std=c++17 -w" }
// { dg-do run }

struct A { };
struct B: A { int i; };
struct C: A, B { int j; };

constexpr C c = { {}, { {}, 1 }, 2 };

#define assert(X) do { if (!(X)) __builtin_abort(); } while(0)
int main()
{
  assert (c.i == 1 && c.j == 2);
}

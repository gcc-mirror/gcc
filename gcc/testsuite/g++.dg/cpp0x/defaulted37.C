// DR 1402
// { dg-do compile { target c++11 } }

struct A
{
  int moved = 0;
  A& operator=(A&&) { ++moved; }
  ~A() { if (moved > 1) __builtin_abort(); }
};

struct B: virtual A { B& operator=(B&&) = default; };
struct C: virtual A { };	// { dg-error "operator=.const A&" }

int main()
{
  B b1, b2;
  b2 = static_cast<B&&>(b1);

  C c1, c2;
  c2 = static_cast<C&&>(c1);	// { dg-error "operator=.const C&" }
}

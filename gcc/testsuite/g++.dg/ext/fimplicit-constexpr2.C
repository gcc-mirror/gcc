// { dg-additional-options -fimplicit-constexpr }
// { dg-do compile { target c++23 } }

class A
{
public:
  A () { asm volatile (""); } // { dg-error {inline assembly is not a constant expression} }
  ~A () {}
};

constexpr bool
test ()
{
  A a; // { dg-error {'A::A\(\)' called in a constant expression} }
  return true;
}

static_assert (test ()); // { dg-error {non-constant condition for static assertion} }

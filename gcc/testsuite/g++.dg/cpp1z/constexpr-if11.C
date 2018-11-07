// Test that discarded statements differ from unevaluated operands in some
// ways.
// { dg-do compile { target c++17 } }

struct A { int i; };

int main()
{
  if constexpr(true)
    ;
  else
    {
      []{}();
      A::i;			// { dg-error "non-static" }
    }
}

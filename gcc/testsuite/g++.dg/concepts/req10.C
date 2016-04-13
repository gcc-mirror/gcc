// { dg-options "-std=c++1z -fconcepts" }

// Test that standard conversions are checked with
// implicit conversion constraints.

template<typename T, typename U>
concept bool C()
{
  return requires(T& t) { {t} -> U&; };
}

struct B { };
class D : B { };

int main()
{
  static_assert(C<D, B>(), ""); // { dg-error "failed" }
}

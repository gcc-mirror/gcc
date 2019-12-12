// Test that we reject a fold-expression with an LHS that is not a
// cast-expression.

// { dg-do compile { target c++17 } }

int i;

template <int... Is>
void f()
{
  (i ? i : Is + ...);		// { dg-error "" }
  (i + Is + ...);		// { dg-error "" }
  (i - Is + ...);		// { dg-error "" }
  (i * Is + ...);		// { dg-error "" }
  (i / Is + ...);		// { dg-error "" }
  (i % Is + ...);		// { dg-error "" }
  (i ^ Is + ...);		// { dg-error "" }
  (i | Is + ...);		// { dg-error "" }
  (i & Is + ...);		// { dg-error "" }
  (i << Is + ...);		// { dg-error "" }
  (i >> Is + ...);		// { dg-error "" }
  (i = Is + ...);		// { dg-error "" }
  (i += Is + ...);		// { dg-error "" }
  (i -= Is + ...);		// { dg-error "" }
  (i *= Is + ...);		// { dg-error "" }
  (i /= Is + ...);		// { dg-error "" }
  (i %= Is + ...);		// { dg-error "" }
  (i ^= Is + ...);		// { dg-error "" }
  (i |= Is + ...);		// { dg-error "" }
  (i &= Is + ...);		// { dg-error "" }
  (i <<= Is + ...);		// { dg-error "" }
  (i >>= Is + ...);		// { dg-error "" }
  (i == Is + ...);		// { dg-error "" }
  (i != Is + ...);		// { dg-error "" }
  (i < Is + ...);		// { dg-error "" }
  (i > Is + ...);		// { dg-error "" }
  (i <= Is + ...);		// { dg-error "" }
  (i >= Is + ...);		// { dg-error "" }
  (i && Is + ...);		// { dg-error "" }
  (i || Is + ...);		// { dg-error "" }
  (i , Is + ...);		// { dg-error "" }
}

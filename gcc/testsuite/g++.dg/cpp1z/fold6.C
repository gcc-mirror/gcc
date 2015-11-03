// Test that we reject a fold-expression with an LHS that is not a
// cast-expression.

// { dg-options -std=c++1z }

int i;

template <int... Is>
int f()
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

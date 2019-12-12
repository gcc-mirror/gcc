// PR c++/87519 - bogus warning with -Wsign-conversion.
// { dg-options "-Wsign-conversion" }

typedef unsigned long int uint64_t;

void f(unsigned long int a, int q)
{
  a += a + q; // { dg-warning "may change the sign" }

  // Explicit cast should disable the warning.
  a = a + static_cast<uint64_t>(q);
  a = a + (uint64_t) q;
  a = a + uint64_t(q);
  a = a + static_cast<const uint64_t>(q);
  a = a + (const uint64_t) q;
  a = a + static_cast<unsigned long int>(q);
  a = a + static_cast<const unsigned long int>(q);
}

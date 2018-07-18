// PR c/35430
// { dg-do compile }
// { dg-options "-Wsign-compare" }

void
foo (__complex__ int i)
{
  i == 0u;
  i == ~1u;	// { dg-warning "comparison of integer expressions of different signedness" }
}

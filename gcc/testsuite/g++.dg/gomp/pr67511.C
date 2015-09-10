// PR c++/67511
// { dg-do compile }
// { dg-options "-fopenmp" }

struct I
{
  I ();
  I (const I &);
  I &operator++ ();
  bool operator< (const I &) const;
};
__PTRDIFF_TYPE__ operator- (const I &, const I &);

void
foo (I &x, I &y)
{
#pragma omp for
  for (I i = x; i < y; ++i)	// { dg-error "no match for" }
    ;
}

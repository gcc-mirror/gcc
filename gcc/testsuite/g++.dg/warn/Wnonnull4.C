// PR c++/86210
// { dg-do compile }
// { dg-options "-Wnonnull" }

void *declared_not_defined (void *p) __attribute__((nonnull));

inline void *declared_and_defined (void *p) __attribute__((nonnull));

int
main ()
{
  int *const p = 0;
  declared_not_defined (p);	// { dg-warning "null argument where non-null required" }
  declared_and_defined (p);	// { dg-warning "null argument where non-null required" }
}

void *
declared_and_defined (void *p)
{
  return p;
}

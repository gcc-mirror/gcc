// PR c++/38348
// { dg-do compile }
// { dg-options "-fopenmp" }

const char *p = "abcde";

template <typename T>
void
f1 (void)
{
#pragma omp for
  for (void *q = (void *)p; q < (void *) (p + 4); q++)	// { dg-error "forbids incrementing a pointer of type" }
    ;
}

template <typename T>
void
f2 (void)
{
#pragma omp for
  for (const char *q = p; q < p + 4; q++)
    ;
}

template <typename T>
void
f3 (void)
{
#pragma omp for
  for (T q = T (p); q < T (p + 4); q++)	// { dg-error "forbids incrementing a pointer of type" }
    ;
}

int
main (void)
{
  f1 <int> ();		// { dg-message "required from here" }
  f2 <int> ();
  f3 <const char *> ();
  f3 <void *> ();	// { dg-message "required from here" }
}

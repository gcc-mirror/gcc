// PR c++/80363
// { dg-do compile }

typedef int V __attribute__((vector_size (16)));

int
foo (V *a, V *b)
{
  if (*a < *b)	// { dg-error "could not convert\[^#]*from" }
    return 1;
  return 0;
}

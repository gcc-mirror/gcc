// Test -Wsizeof-pointer-memaccess warnings.
// { dg-do compile }
// { dg-options "-Wall -Wno-sizeof-array-argument" }

typedef __SIZE_TYPE__ size_t;
extern "C" void *memset (void *, int, size_t);

int
foo (int x, char b[10])
{
  long a[memset (b, 0, sizeof (b)) ? x + 10 : x];	// { dg-warning "call is the same expression as the destination; did you mean to provide an explicit length?" }
  return a[0];
}

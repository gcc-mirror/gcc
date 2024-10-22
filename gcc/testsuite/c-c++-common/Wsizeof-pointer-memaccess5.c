/* PR c/117230 */
/* { dg-do compile } */
/* { dg-options "-Wsizeof-pointer-memaccess" } */

typedef int V __attribute__((vector_size (sizeof (int))));

void
foo (V *a, char *b)
{
  __builtin_memcpy (b, a, sizeof (a));		/* { dg-warning "argument to 'sizeof' in '\[^\n\r]*__builtin_memcpy\[^\n\r]*' call is the same expression as the source; did you mean to dereference it\\\?" } */
}

void
bar (V *a, char *b)
{
  __builtin_memcpy (a, b, sizeof (a));		/* { dg-warning "argument to 'sizeof' in '\[^\n\r]*__builtin_memcpy\[^\n\r]*' call is the same expression as the destination; did you mean to dereference it\\\?" } */
}

int
baz (V *a, char *b)
{
  return __builtin_memcmp (a, b, sizeof (a));	/* { dg-warning "argument to 'sizeof' in '\[^\n\r]*__builtin_memcmp\[^\n\r]*' call is the same expression as the first source; did you mean to dereference it\\\?" } */
}

int
qux (V *a, char *b)
{
  return __builtin_memcmp (b, a, sizeof (a));	/* { dg-warning "argument to 'sizeof' in '\[^\n\r]*__builtin_memcmp\[^\n\r]*' call is the same expression as the second source; did you mean to dereference it\\\?" } */
}

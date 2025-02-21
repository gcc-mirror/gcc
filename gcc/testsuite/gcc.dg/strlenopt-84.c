/* PR tree-optimization/83821 - local aggregate initialization defeats
   strlen optimization
   Verify that stores that overwrite an interior nul are correctly
   reflected in strlen results.
   { dg-do run }
   { dg-options "-O2 -Wall" } */

#define false (0 == 1)
#define true (0 == 0)
#define assert(e)							\
  ((e) ? (void)0 : (__builtin_printf ("assertion failed on line %i\n",	\
				      __LINE__), __builtin_abort ()))

#define ATTR(...) __attribute__ ((__VA_ARGS__))

static inline int ATTR (always_inline)
assign_and_get_length (char *p, _Bool clear)
{
  p[0] = 'a';

  if (clear)
    p[1] = 0;

  p[2] = 'c';

  if (clear)
    p[3] = 0;

  p[1] = 'b';

  return __builtin_strlen (p);
}

ATTR (noipa) void array_get_length (void)
{
  char a[4];
  unsigned n = assign_and_get_length (a, true);
  assert (n == 3);
}

ATTR (noipa) void clear_array_get_length (void)
{
  char a[4] = { };
  unsigned n = assign_and_get_length (a, false);
  assert (n == 3);
}

ATTR (noipa) void calloc_get_length (void)
{
  char *p = __builtin_calloc (5, 1);
  unsigned n = assign_and_get_length (p, false);
  assert (n == 3);
}

ATTR (noipa) void malloc_get_length (void)
{
  char *p = __builtin_malloc (5);
  unsigned n = assign_and_get_length (p, true);
  assert (n == 3);
}

ATTR (noipa) void vla_get_length (int n)
{
  char a[n];
  unsigned len = assign_and_get_length (a, true);
  assert (len == 3);
}


static inline void ATTR (always_inline)
assign_and_test_length (char *p, _Bool clear)
{
  p[0] = 'a';

  if (clear)
    p[1] = 0;

  p[2] = 'c';

  if (clear)
    p[3] = 0;

  unsigned n0 =  __builtin_strlen (p);

  p[1] = 'b';

  unsigned n1 =  __builtin_strlen (p);
  assert (n0 != n1);
}

ATTR (noipa) void array_test_length (void)
{
  char a[4];
  assign_and_test_length (a, true);
}

ATTR (noipa) void clear_array_test_length (void)
{
  char a[4] = { };
  assign_and_test_length (a, false);
}

ATTR (noipa) void calloc_test_length (void)
{
  char *p = __builtin_calloc (5, 1);
  assign_and_test_length (p, false);
}

ATTR (noipa) void malloc_test_length (void)
{
  char *p = __builtin_malloc (5);
  assign_and_test_length (p, true);
}

ATTR (noipa) void vla_test_length (int n)
{
  char a[n];
  assign_and_test_length (a, true);
}

int main (void)
{
  array_get_length ();
  clear_array_get_length ();
  calloc_get_length ();
  malloc_get_length ();
  vla_get_length (4);

  array_test_length ();
  clear_array_test_length ();
  calloc_test_length ();
  malloc_test_length ();
  vla_test_length (4);
}


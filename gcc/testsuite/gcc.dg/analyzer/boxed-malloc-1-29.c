/* Isolating this false positive from boxed-malloc-1.c since it's
   reported within boxed_malloc.  */

#include <stdlib.h>

typedef struct boxed_ptr { void *value; } boxed_ptr;

boxed_ptr
boxed_malloc (size_t sz)
{
  boxed_ptr result;
  result.value = malloc (sz);
  return result; /* { dg-bogus "leak" "leak false +ve (PR analyzer/104979)" { xfail *-*-* } } */
}

boxed_ptr
boxed_free (boxed_ptr ptr)
{
  free (ptr.value);
}

const boxed_ptr boxed_null = {NULL};

struct link
{
  boxed_ptr m_ptr;
};

boxed_ptr test_29 (void)
{
  boxed_ptr res = boxed_malloc (sizeof (struct link));
  if (!res.value)
    return boxed_null;
  ((struct link *)res.value)->m_ptr = boxed_malloc (sizeof (struct link));
  return res;
}

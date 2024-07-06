/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } { "" } } */
/* { dg-require-effective-target size24plus } */

#include <stdlib.h>
#include "../analyzer-decls.h"

typedef struct boxed_ptr { void *value; } boxed_ptr;

boxed_ptr __attribute__((noinline))
boxed_malloc (size_t sz)
{
  boxed_ptr result;
  result.value = malloc (sz);
  return result;
}

boxed_ptr __attribute__((noinline))
boxed_free (boxed_ptr ptr)
{
  free (ptr.value);
}

const boxed_ptr boxed_null = {NULL};

boxed_ptr test_1 (int flag)
{
  boxed_ptr ptr = boxed_malloc (sizeof (int));

  if (flag) /* { dg-message "following 'false' branch" } */
    if (!ptr.value)
      return boxed_null;

  *((int *)ptr.value) = 42; /* { dg-warning "dereference of possibly-NULL '\[^\n\r\]*'" } */

  return ptr;
}

void test_2 (int flag)
{
  boxed_ptr ptr;

  if (flag)
    ptr = boxed_malloc (4096);
  else
    ptr = boxed_malloc (1024);

  __analyzer_dump_exploded_nodes (0); /* { dg-warning "2 processed enodes" } */

  boxed_free (ptr);  

  __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 processed enode" } */
}

void test_3 (int kind)
{
  boxed_ptr ptr;

  switch (kind)
    {
    default:
      ptr = boxed_malloc (4096);
      break;
    case 0:
      ptr = boxed_malloc (128);
      break;
    case 1:
      ptr = boxed_malloc (1024);
      break;
    case 2:
      ptr = boxed_malloc (65536);
      break;
    }

  __analyzer_dump_exploded_nodes (0); /* { dg-warning "4 processed enodes" } */

  boxed_free (ptr);  

  __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 processed enode" } */
}

typedef __SIZE_TYPE__ size_t;
#include "../../gcc.dg/analyzer/analyzer-decls.h"

/* Duplicating a string.  */

/* Correct but poor implementation with repeated __builtin_strlen calls.  */

char *
alloc_dup_1_correct (const char *x)
{
  size_t sz = __builtin_strlen (x) + 1;
  char *result = (char *) __builtin_malloc (sz);
  if (!result)
    return NULL;
  __builtin_memcpy (result, x, __builtin_strlen (x));
  result[__builtin_strlen(x)] = '\0';
  return result;
}

/* Incorrect version: forgetting to add space for terminator.  */

char *
alloc_dup_1_incorrect (const char *x, const char *y)
{
  /* Forgetting to add space for the terminator here.  */
  size_t sz = __builtin_strlen (x) + 1;
  char *result = (char *) __builtin_malloc (sz);
  if (!result)
    return NULL;
  __builtin_memcpy (result, x, __builtin_strlen (x));
  result[__builtin_strlen(x)] = '\0'; /* { dg-warning "heap-based buffer overflow" "PR analyzer/105899" { xfail *-*-* } } */
  return result;
}

/* As above, but only calling __builtin_strlen once.  */

char *
alloc_dup_2_correct (const char *x)
{
  size_t len_x = __builtin_strlen (x);
  size_t sz = len_x + 1;
  char *result = (char *) __builtin_malloc (sz);
  if (!result)
    return NULL;
  __builtin_memcpy (result, x, len_x);
  result[len_x] = '\0';
  return result;
}

char *
alloc_dup_of_concatenated_pair_2_incorrect (const char *x, const char *y)
{
  size_t len_x = __builtin_strlen (x);
  size_t sz = len_x; /* Forgetting to add space for the terminator.  */
  char *result = (char *) __builtin_malloc (sz); /* { dg-message "capacity: 'len_x' bytes" } */
  if (!result)
    return NULL;
  __builtin_memcpy (result, x, len_x);
  result[len_x] = '\0'; /* { dg-warning "heap-based buffer overflow" } */
  return result;
}

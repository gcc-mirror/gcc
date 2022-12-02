typedef __SIZE_TYPE__ size_t;
#define NULL ((void *)0)

/* Concatenating a pair of strings.  */

/* Correct but poor implementation with repeated __builtin_strlen calls.  */

char *
alloc_dup_of_concatenated_pair_1_correct (const char *x, const char *y)
{
  size_t sz = __builtin_strlen (x) + __builtin_strlen (y) + 1;
  char *result = __builtin_malloc (sz);
  if (!result)
    return NULL;
  __builtin_memcpy (result, x, __builtin_strlen (x));
  __builtin_memcpy (result + __builtin_strlen (x), y, __builtin_strlen (y));
  result[__builtin_strlen(x) + __builtin_strlen (y)] = '\0';
  return result;
}

/* Incorrect version: forgetting to add space for terminator.  */

char *
alloc_dup_of_concatenated_pair_1_incorrect (const char *x, const char *y)
{
  /* Forgetting to add space for the terminator here.  */
  size_t sz = __builtin_strlen (x) + __builtin_strlen (y);
  char *result = __builtin_malloc (sz);
  if (!result)
    return NULL;
  __builtin_memcpy (result, x, __builtin_strlen (x));
  __builtin_memcpy (result + __builtin_strlen (x), y, __builtin_strlen (y));
  result[__builtin_strlen(x) + __builtin_strlen (y)] = '\0'; /* { dg-warning "heap-based buffer overflow" "PR analyzer/105899" { xfail *-*-* } } */
  return result;
}

/* As above, but only calling __builtin_strlen once on each input.  */

char *
alloc_dup_of_concatenated_pair_2_correct (const char *x, const char *y)
{
  size_t len_x = __builtin_strlen (x);
  size_t len_y = __builtin_strlen (y);
  size_t sz = len_x + len_y + 1;
  char *result = __builtin_malloc (sz);
  if (!result)
    return NULL;
  __builtin_memcpy (result, x, len_x);
  __builtin_memcpy (result + len_x, y, len_y);
  result[len_x + len_y] = '\0';
  return result;
}

char *
alloc_dup_of_concatenated_pair_2_incorrect (const char *x, const char *y)
{
  size_t len_x = __builtin_strlen (x);
  size_t len_y = __builtin_strlen (y);
  size_t sz = len_x + len_y; /* Forgetting to add space for the terminator.  */
  char *result = __builtin_malloc (sz); /* { dg-message "capacity: 'len_x \\+ len_y' bytes" } */
  if (!result)
    return NULL;
  __builtin_memcpy (result, x, len_x);
  __builtin_memcpy (result + len_x, y, len_y);
  result[len_x + len_y] = '\0'; /* { dg-warning "heap-based buffer overflow" } */
  return result;
}

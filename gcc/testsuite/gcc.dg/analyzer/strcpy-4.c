/* { dg-additional-options "-Wno-stringop-overflow" } */

#include <string.h>
#include "analyzer-decls.h"

void
test_fixed_size_stack_1 (void)
{
  char buf[3];
  strcpy (buf, "abc"); /* { dg-warning "stack-based buffer overflow" } */
}

char *test_fixed_size_heap_1 (void)
{
  char str[] = "abc";
  char *p = __builtin_malloc (3);
  if (!p)
    return NULL;
  strcpy (p, str); /* { dg-warning "heap-based buffer overflow" } */
  return p;
}

char *test_fixed_size_heap_2_invalid (void)
{
  char str[] = "abc";
  char *p = __builtin_malloc (strlen (str));
  if (!p)
    return NULL;
  strcpy (p, str); /* { dg-warning "heap-based buffer overflow" } */
  return p;
}

char *test_fixed_size_heap_2_valid (void)
{
  char str[] = "abc";
  char *p = __builtin_malloc (strlen (str) + 1);
  if (!p)
    return NULL;
  strcpy (p, str); /* { dg-bogus "" } */
  __analyzer_eval (strlen (p) == 3); /* { dg-warning "TRUE" } */
  return p;
}

char *test_dynamic_size_heap_1 (const char *src)
{
  char *p = __builtin_malloc (strlen (src));
  if (!p)
    return NULL;
  strcpy (p, src); // TODO: write of null terminator is oob
  return p;
}

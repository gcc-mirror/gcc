/* Test for the "nonnull" function attribute on builtins.  Use the
   "__builtin_" style below so we don't need prototypes.  */
/* { dg-do compile } */
/* { dg-options "-Wnonnull" } */

#include <stddef.h>

void
use (void *x, size_t y)
{
  (void) x;
  (void) y;
}

void *
foo (void *p, char *s)
{
  __builtin_bzero (NULL, 0);
  __builtin_bcopy (NULL, p, 0);
  __builtin_bcopy (p, NULL, 0);
  __builtin_bcmp (NULL, p, 0);
  __builtin_bcmp (p, NULL, 0);

  __builtin_memcpy (p, NULL, 0);
  __builtin_memcpy (NULL, p, 0);
  __builtin_memmove (p, NULL, 0);
  __builtin_memmove (NULL, p, 0);
  __builtin_memcmp (p, NULL, 0);
  __builtin_memcmp (NULL, p, 0);
  __builtin_memset (NULL, 0, 0);
  __builtin_mempcpy (p, NULL, 0);
  __builtin_mempcpy (NULL, p, 0);

  __builtin_strncat (NULL, s, 0);  /* { dg-warning "null" "null pointer check" } */
  __builtin_strncat (s, NULL, 0);
  __builtin_stpncpy (NULL, s, 0);
  __builtin_stpncpy (s, NULL, 0);
  __builtin_strncpy (NULL, s, 0);
  __builtin_strncpy (s, NULL, 0);
  __builtin_strncasecmp (NULL, s, 0);
  __builtin_strncasecmp (s, NULL, 0);
  __builtin_strncmp (NULL, s, 0);
  __builtin_strncmp (s, NULL, 0);
  void *p1 = __builtin_strndup (NULL, 0);

  size_t n = __builtin_strnlen (NULL, 16);  /* { dg-warning "null" "null pointer check" } */
  use (NULL, n);
  n = __builtin_strnlen (NULL, 0);
  use (NULL, n);

  void *q = __builtin_memchr (NULL, ' ', 16);  /* { dg-warning "null" "null pointer check" } */
  use (q, 0);
  q = __builtin_memchr (NULL, ' ', 0);
  use (q, 0);
  return p1;
}

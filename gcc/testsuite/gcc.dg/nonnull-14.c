/* Test for the "nonnull" function attribute on builtins.  Use the
   "__builtin_" style below so we don't need prototypes.  */
/* { dg-do compile } */
/* { dg-options "-Wnonnull" } */

#include <stddef.h>

void
foo (void *p, char *s)
{
  __builtin_fwrite (s, 0, 0, NULL);  /* { dg-warning "null" "null pointer check" } */
  __builtin_fwrite (s, 0, 2, NULL);  /* { dg-warning "null" "null pointer check" } */
  __builtin_fwrite (s, 1, 0, NULL);  /* { dg-warning "null" "null pointer check" } */
  __builtin_fwrite (NULL, 16, 0, p);
  __builtin_fwrite (NULL, 0, 12, p);
  __builtin_fwrite (NULL, 2, 3, p);  /* { dg-warning "null" "null pointer check" } */
  __builtin_fwrite_unlocked (s, 0, 0, NULL);  /* { dg-warning "null" "null pointer check" } */
  __builtin_fwrite_unlocked (s, 0, 2, NULL);  /* { dg-warning "null" "null pointer check" } */
  __builtin_fwrite_unlocked (s, 1, 0, NULL);  /* { dg-warning "null" "null pointer check" } */
  __builtin_fwrite_unlocked (NULL, 16, 0, p);
  __builtin_fwrite_unlocked (NULL, 0, 12, p);
  __builtin_fwrite_unlocked (NULL, 2, 3, p);  /* { dg-warning "null" "null pointer check" } */
}

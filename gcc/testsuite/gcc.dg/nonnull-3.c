/* Test for the "nonnull" function attribute on builtins.  Use the
   "__builtin_" style below so we don't need prototypes.  */
/* Origin: Kaveh R. Ghazi <ghazi@caip.rutgers.edu> */
/* { dg-do compile } */
/* { dg-options "-Wnonnull" } */

#include <stddef.h>

void
foo (void *p, char *s)
{
  __builtin_bzero (NULL, 0);
  __builtin_bcopy (NULL, p, 0);
  __builtin_bcopy (p, NULL, 0);
  __builtin_bcmp (NULL, p, 0);
  __builtin_bcmp (p, NULL, 0);
  __builtin_index (NULL, 16);  /* { dg-warning "null" "null pointer check" } */
  __builtin_rindex (NULL, 16);  /* { dg-warning "null" "null pointer check" } */

  __builtin_memcpy (p, NULL, 16);  /* { dg-warning "null" "null pointer check" } */
  __builtin_memcpy (NULL, p, 16);  /* { dg-warning "null" "null pointer check" } */
  __builtin_memmove (p, NULL, 16);  /* { dg-warning "null" "null pointer check" } */
  __builtin_memmove (NULL, p, 16);  /* { dg-warning "null" "null pointer check" } */
  __builtin_memcmp (p, NULL, 16);  /* { dg-warning "null" "null pointer check" } */
  __builtin_memcmp (NULL, p, 16);  /* { dg-warning "null" "null pointer check" } */
  __builtin_memset (NULL, 0, 16);  /* { dg-warning "null" "null pointer check" } */
  __builtin_mempcpy (p, NULL, 16);  /* { dg-warning "null" "null pointer check" } */
  __builtin_mempcpy (NULL, p, 16);  /* { dg-warning "null" "null pointer check" } */

  __builtin_strcat (NULL, s);  /* { dg-warning "null" "null pointer check" } */
  __builtin_strcat (s, NULL);  /* { dg-warning "null" "null pointer check" } */
  __builtin_strncat (NULL, s, 16);  /* { dg-warning "null" "null pointer check" } */
  __builtin_strncat (s, NULL, 16);  /* { dg-warning "null" "null pointer check" } */
  __builtin_stpcpy (NULL, s);  /* { dg-warning "null" "null pointer check" } */
  __builtin_stpcpy (s, NULL);  /* { dg-warning "null" "null pointer check" } */
  __builtin_strcpy (NULL, s);  /* { dg-warning "null" "null pointer check" } */
  __builtin_strcpy (s, NULL);  /* { dg-warning "null" "null pointer check" } */
  __builtin_strncpy (NULL, s, 16);  /* { dg-warning "null" "null pointer check" } */
  __builtin_strncpy (s, NULL, 16);  /* { dg-warning "null" "null pointer check" } */
  __builtin_strcmp (NULL, s);  /* { dg-warning "null" "null pointer check" } */
  __builtin_strcmp (s, NULL);  /* { dg-warning "null" "null pointer check" } */
  __builtin_strncmp (NULL, s, 16);  /* { dg-warning "null" "null pointer check" } */
  __builtin_strncmp (s, NULL, 16);  /* { dg-warning "null" "null pointer check" } */
  __builtin_strlen (NULL);  /* { dg-warning "null" "null pointer check" } */
  __builtin_strstr (NULL, s);  /* { dg-warning "null" "null pointer check" } */
  __builtin_strstr (s, NULL);  /* { dg-warning "null" "null pointer check" } */
  __builtin_strpbrk (NULL, s);  /* { dg-warning "null" "null pointer check" } */
  __builtin_strpbrk (s, NULL);  /* { dg-warning "null" "null pointer check" } */
  __builtin_strspn (NULL, s);  /* { dg-warning "null" "null pointer check" } */
  __builtin_strspn (s, NULL);  /* { dg-warning "null" "null pointer check" } */
  __builtin_strchr (NULL, 16);  /* { dg-warning "null" "null pointer check" } */
  __builtin_strrchr (NULL, 16);  /* { dg-warning "null" "null pointer check" } */
  __builtin_strdup (NULL);  /* { dg-warning "null" "null pointer check" } */

  __builtin_nan (NULL);  /* { dg-warning "null" "null pointer check" } */
  __builtin_nanf (NULL);  /* { dg-warning "null" "null pointer check" } */
  __builtin_nanl (NULL);  /* { dg-warning "null" "null pointer check" } */
  __builtin_nans (NULL);  /* { dg-warning "null" "null pointer check" } */
  __builtin_nansf (NULL);  /* { dg-warning "null" "null pointer check" } */
  __builtin_nansl (NULL);  /* { dg-warning "null" "null pointer check" } */

  __builtin_puts (NULL);  /* { dg-warning "null" "null pointer check" } */
  __builtin_fputc (*s, NULL);  /* { dg-warning "null" "null pointer check" } */
  __builtin_fputs (s, NULL);  /* { dg-warning "null" "null pointer check" } */
  __builtin_fputs (NULL, p);  /* { dg-warning "null" "null pointer check" } */
  __builtin_fwrite (s, 16, 16, NULL);  /* { dg-warning "null" "null pointer check" } */
  __builtin_fwrite (NULL, 16, 16, p);  /* { dg-warning "null" "null pointer check" } */
  __builtin_puts_unlocked (NULL);  /* { dg-warning "null" "null pointer check" } */
  __builtin_fputc_unlocked (*s, NULL);  /* { dg-warning "null" "null pointer check" } */
  __builtin_fputs_unlocked (s, NULL);  /* { dg-warning "null" "null pointer check" } */
  __builtin_fputs_unlocked (NULL, p);  /* { dg-warning "null" "null pointer check" } */
  __builtin_fwrite_unlocked (s, 16, 16, NULL);  /* { dg-warning "null" "null pointer check" } */
  __builtin_fwrite_unlocked (NULL, 16, 16, p);  /* { dg-warning "null" "null pointer check" } */

}

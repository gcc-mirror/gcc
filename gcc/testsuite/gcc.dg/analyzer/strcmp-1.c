/* Verify that we clarify the sense of paths involving strcmp.  */

#include <string.h>
#include <stdlib.h>

void test_1 (const char *str, char *ptr)
{
  if (strcmp (str, "VALUE")) /* { dg-message "following 'true' branch \\(when the strings are non-equal\\)\\.\\.\\." } */
    free (ptr);
  free (ptr); /* { dg-warning "double-'free' of 'ptr'" } */
}

void test_2 (const char *str, char *ptr)
{
  if (strcmp (str, "VALUE") == 0) /* { dg-message "following 'true' branch \\(when the strings are equal\\)\\.\\.\\." } */
    free (ptr);
  free (ptr); /* { dg-warning "double-'free' of 'ptr'" } */
}

void test_3 (const char *str, char *ptr)
{
  if (!strcmp (str, "VALUE")) /* { dg-message "following 'true' branch \\(when the strings are equal\\)\\.\\.\\." } */
    free (ptr);
  free (ptr); /* { dg-warning "double-'free' of 'ptr'" } */
}

void test_4 (const char *str, char *ptr)
{
  if (strcmp (str, "VALUE")) /* { dg-message "following 'false' branch \\(when the strings are equal\\)\\.\\.\\." } */
    {
    }
  else
    free (ptr);
  free (ptr); /* { dg-warning "double-'free' of 'ptr'" } */
}

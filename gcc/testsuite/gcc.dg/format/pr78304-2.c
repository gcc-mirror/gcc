/* { dg-do compile } */
/* { dg-options "-O2 -Wall -Wextra" } */

extern int printf (const char *, ...);

# define PRIu32		"u"

void test (const char *msg)
{
  printf ("size: %" PRIu32 "\n", msg); /* { dg-warning "expects argument of type" } */
}

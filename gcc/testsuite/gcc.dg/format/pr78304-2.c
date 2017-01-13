/* { dg-do compile } */
/* { dg-options "-O2 -Wall -Wextra" } */

extern int printf (const char *, ...);

# define PRIu32		"u"

void test (long size)
{
  printf ("size: %" PRIu32 "\n", size); /* { dg-warning "expects argument of type" } */
}

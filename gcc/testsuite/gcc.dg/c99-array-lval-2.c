/* Test for non-lvalue arrays decaying to pointers: in C99 only.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

struct s { char c[17]; };

struct s x;

extern struct s foo (void);

#define ASSERT(v, a)	char v[((a) ? 1 : -1)]

ASSERT (p, sizeof (x.c) == 17);
ASSERT (q, sizeof (0, x.c) == sizeof (char *));
ASSERT (r, sizeof ((foo ()).c) == 17);
/* The non-lvalue array decays to a pointer in C99.  */
ASSERT (s, sizeof (0, (foo ()).c) == sizeof (char *)); /* { dg-bogus "array" "bad non-lvalue array handling" } */

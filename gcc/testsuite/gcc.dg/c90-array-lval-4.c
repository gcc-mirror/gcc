/* Test for non-lvalue arrays decaying to pointers: in C99 only.
   Test various ways of producing non-lvalue arrays.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1990 -pedantic-errors" } */

struct s { char c[17]; };

struct s x;

struct s a, b, c;
int d;

#define ASSERT(v, a)	char v[((a) ? 1 : -1)]

ASSERT (p, sizeof (x.c) == 17);
ASSERT (q, sizeof (0, x.c) == sizeof (char *));
ASSERT (r0, sizeof ((d ? b : c).c) == 17);
ASSERT (r1, sizeof ((d, b).c) == 17);
ASSERT (r2, sizeof ((a = b).c) == 17);
/* The non-lvalue array does not decay to a pointer, so the comma expression
   has (non-lvalue) array type.
*/
ASSERT (s0, sizeof (0, (d ? b : c).c) == 17); /* { dg-bogus "array" "bad non-lvalue array handling" } */
ASSERT (s0, sizeof (0, (d, b).c) == 17); /* { dg-bogus "array" "bad non-lvalue array handling" } */
ASSERT (s0, sizeof (0, (a = b).c) == 17); /* { dg-bogus "array" "bad non-lvalue array handling" } */

/* Test for rejection of taking address of bit-fields.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "" } */

#include <stddef.h>

struct s { int a : 1; } x, *y;

int a = offsetof (struct s, a); /* { dg-error "attempt to take address of bit-field structure member 'a'" } */
void *b = &x.a; /* { dg-error "cannot take address of bit-field 'a'" } */
void *c = &y->a; /* { dg-error "cannot take address of bit-field 'a'" } */

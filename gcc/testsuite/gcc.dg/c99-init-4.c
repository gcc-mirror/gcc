/* Test for nested initialization of a compound literal: must not be
   checked against outer array bounds.  Bug 19435.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

struct s { char *p; };
struct s a[1] = { { .p = ((char []){ 1, 2 }) } };

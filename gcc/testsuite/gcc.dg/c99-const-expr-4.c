/* Test for constant expressions: const variable with value 0 is not a
   null pointer constant so the conditional expression should have
   type void * and the assignment is OK.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -O2" } */
int *p;
long *q;
static void *const n = 0;
int j;
void f(void) { q = j ? p : n; }

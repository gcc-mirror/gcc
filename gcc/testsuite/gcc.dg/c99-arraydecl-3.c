/* Test for C99 forms of array declarator.  Test restrict qualifiers
   properly applied to type of parameter.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

void
f0 (int a[restrict])
{
  int **b = &a; /* { dg-error "discards 'restrict' qualifier" } */
  int *restrict *c = &a;
}

void
f1 (a)
     int a[restrict];
{
  int **b = &a; /* { dg-error "discards 'restrict' qualifier" } */
  int *restrict *c = &a;
}

/* Test for anonymous structures and unions not permitted in C99.  */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

struct s
{
  int a;
  struct
  {
    int b;
  }; /* { dg-error "unnamed structs" } */
};

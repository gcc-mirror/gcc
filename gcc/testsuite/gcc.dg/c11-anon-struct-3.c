/* Test for anonymous structures and unions in C11.  Test for invalid
   cases: typedefs disallowed by N1549.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic-errors" } */

typedef struct
{
  int i;
} s0;

typedef union
{
  int i;
} u0;

struct s1
{
  int a;
  u0; /* { dg-error "declaration does not declare anything" } */
  struct
  {
    int b;
  };
};

union u1
{
  int b;
  s0; /* { dg-error "declaration does not declare anything" } */
  union
  {
    int c;
  };
};

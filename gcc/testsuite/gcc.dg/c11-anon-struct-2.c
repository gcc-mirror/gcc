/* Test for anonymous structures and unions in C11.  Test for invalid
   cases.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic-errors" } */

typedef struct s0
{
  int i;
} s0;

struct s1
{
  int a;
  struct s0; /* { dg-error "declaration does not declare anything" } */
};

struct s2
{
  int a;
  s0; /* { dg-error "declaration does not declare anything" } */
};

struct s3
{
  struct
  {
    int i;
  };
  struct
  {
    int i; /* { dg-error "duplicate member" } */
  };
};

struct s4
{
  int a;
  struct s
  {
    int i;
  }; /* { dg-error "declaration does not declare anything" } */
};

struct s5
{
  struct
  {
    int i;
  } a;
  int b;
} x;

void
f (void)
{
  x.i = 0; /* { dg-error "has no member" } */
}

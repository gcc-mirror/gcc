/* Test for anonymous structures and unions in C1X.  */
/* { dg-do compile } */
/* { dg-options "-std=c1x -pedantic-errors" } */

#include <stddef.h>

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
  u0;
  struct
  {
    int b;
  };
};

union u1
{
  int b;
  s0;
  union
  {
    int c;
  };
};

struct s2
{
  struct
  {
    int a;
  };
};

struct s3
{
  u0;
};

struct s4
{
  struct
  {
    int i;
  };
  int a[];
};

struct s1 x =
  {
    .b = 1,
    .i = 2,
    .a = 3
  };

int o = offsetof (struct s1, i);

void
f (void)
{
  x.i = 3;
  (&x)->i = 4;
}

/* { dg-do compile } */
/* { dg-options "-Wlogical-not-parentheses" } */

/* Test that we don't warn if rhs is 0 and comparison is == or !=.  */

#ifndef __cplusplus
#define bool _Bool
#endif

bool r;

int
f1 (int a)
{
  r = !a == 0;
  r = !a != 0;
  r = !a == 1;	/* { dg-warning "logical not is only applied to the left hand side of comparison" } */
  r = !a != 1;	/* { dg-warning "logical not is only applied to the left hand side of comparison" } */
}

int
f2 (int a)
{
  r = !a > 0;	/* { dg-warning "logical not is only applied to the left hand side of comparison" } */
  r = !a >= 0;	/* { dg-warning "logical not is only applied to the left hand side of comparison" } */
  r = !a < 0;	/* { dg-warning "logical not is only applied to the left hand side of comparison" } */
  r = !a <= 0;	/* { dg-warning "logical not is only applied to the left hand side of comparison" } */
  r = !a > 1;	/* { dg-warning "logical not is only applied to the left hand side of comparison" } */
  r = !a >= 1;	/* { dg-warning "logical not is only applied to the left hand side of comparison" } */
  r = !a < 1;	/* { dg-warning "logical not is only applied to the left hand side of comparison" } */
  r = !a <= 1;	/* { dg-warning "logical not is only applied to the left hand side of comparison" } */
}

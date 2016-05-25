/* PR c/64249 */
/* { dg-do compile } */
/* { dg-options "-Wduplicated-cond" } */

#ifndef __cplusplus
# define bool _Bool
# define true 1
# define false 0
#endif

extern int foo (void);

int
fn1 (int n)
{
  if (n == 1) /* { dg-message "previously used here" } */
    return -1;
  else if (n == 2)
    return 0;
  else if (n == 1) /* { dg-warning "duplicated .if. condition" } */
    return 1;
  return 0;
}

int
fn2 (void)
{
  if (4)
    return 1;
  else if (4)
    return 2;

#define N 10
  if (N)
    return 3;
  else if (N)
    return 4;
}

int
fn3 (int n)
{
  if (n == 42)
    return 1;
  if (n == 42)
    return 2;

  if (n)
    if (n)
      if (n)
	if (n)
	  return 42;

  if (!n)
    return 10;
  else
    return 11;
}

int
fn4 (int n)
{
  if (n > 0)
    {
      if (n == 1) /* { dg-message "previously used here" } */
	return 1;
      else if (n == 1) /* { dg-warning "duplicated .if. condition" } */
	return 2;
    }
  else if (n < 0)
    {
      if (n < -1)
	return 6;
      else if (n < -2)
	{
	  if (n == -10) /* { dg-message "previously used here" } */
	    return 3;
	  else if (n == -10) /* { dg-warning "duplicated .if. condition" } */
	    return 4;
	}
    }
  else
    return 7;
  return 0;
}

struct S { long p, q; };

int
fn5 (struct S *s)
{
  if (!s->p) /* { dg-message "previously used here" } */
    return 12345;
  else if (!s->p) /* { dg-warning "duplicated .if. condition" } */
    return 1234;
  return 0;
}

int
fn6 (int n)
{
  if (n) /* { dg-message "previously used here" } */
    return n;
  else if (n) /* { dg-warning "duplicated .if. condition" } */
    return n;
  else if (n) /* { dg-warning "duplicated .if. condition" } */
    return n;
  else if (n) /* { dg-warning "duplicated .if. condition" } */
    return n;
  else if (n) /* { dg-warning "duplicated .if. condition" } */
    return n;
  else if (n) /* { dg-warning "duplicated .if. condition" } */
    return n;
  else if (n) /* { dg-warning "duplicated .if. condition" } */
    return n;
  else if (n) /* { dg-warning "duplicated .if. condition" } */
    return n;
  return 0;
}

int
fn7 (int n)
{
  if (n == 0) /* { dg-message "previously used here" } */
    return 10;
  else if (n == 1) /* { dg-message "previously used here" } */
    return 11;
  else if (n == 2) /* { dg-message "previously used here" } */
    return 12;
  else if (n == 3) /* { dg-message "previously used here" } */
    return 13;
  else if (n == 4) /* { dg-message "previously used here" } */
    return 14;
  else if (n == 5) /* { dg-message "previously used here" } */
    return 15;
  else if (n == 6) /* { dg-message "previously used here" } */
    return 16;
  else if (n == 7) /* { dg-message "previously used here" } */
    return 17;
  else if (n == 0) /* { dg-warning "duplicated .if. condition" } */
    return 100;
  else if (n == 1) /* { dg-warning "duplicated .if. condition" } */
    return 101;
  else if (n == 2) /* { dg-warning "duplicated .if. condition" } */
    return 102;
  else if (n == 3) /* { dg-warning "duplicated .if. condition" } */
    return 103;
  else if (n == 4) /* { dg-warning "duplicated .if. condition" } */
    return 104;
  else if (n == 5) /* { dg-warning "duplicated .if. condition" } */
    return 105;
  else if (n == 6) /* { dg-warning "duplicated .if. condition" } */
    return 106;
  else if (n == 7) /* { dg-warning "duplicated .if. condition" } */
    return 107;
  return 0;
}

int
fn8 (bool b)
{
  if (!b) /* { dg-message "previously used here" } */
    return 16;
  else if (!b) /* { dg-warning "duplicated .if. condition" } */
    return 27;
  else
    return 64;
}

int
fn9 (int i, int j, int k)
{
  if (i > 0 && j > 0 && k > 0) /* { dg-message "previously used here" } */
    return -999;
  else
  if (i > 0 && j > 0 && k > 0) /* { dg-warning "duplicated .if. condition" } */
    return 999;
  else
    return 0;
}

int
fn10 (void)
{
  if (foo ())
    return 17329;
  else if (foo ())
    return 18409;
  return 0;
}

int
fn11 (int n)
{
  if (++n == 10)
    return 666;
  else if (++n == 10)
    return 9;
  return 0;
}

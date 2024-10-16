/* PR c/6940 */
/* { dg-do compile } */
/* { dg-additional-options "-Wno-old-style-definition" { target c } } */

/* Test -Wsizeof-array-argument warning.  */

typedef int T[2][2];

int
fn1 (int a[])
{
  return sizeof a; /* { dg-warning "on array function parameter" } */
}

int
fn2 (int x, int b[3])
{
  return x + sizeof b; /* { dg-warning "on array function parameter" } */
}

int
fn3 (int *p)
{
  return sizeof p;
}

int fn4 (int *p);
int
fn4 (int p[])
{
  return sizeof p; /* { dg-warning "on array function parameter" } */
}

int fn5 (int x[]);
int
fn5 (int *x)
{
  return sizeof x;
}

#ifndef __cplusplus
/* C++ doesn't know VLA unspec.  */
int fn6 (int x[*]);
int
fn6 (int x[])
{
  return sizeof x; /* { dg-warning "on array function parameter" "" { target c } } */
}
#endif

int
fn7 (int x[][2])
{
  return sizeof x; /* { dg-warning "on array function parameter" } */
}

int
fn8 (char *x[])
{
  return sizeof x; /* { dg-warning "on array function parameter" } */
}

int
fn9 (char **x)
{
  return sizeof x;
}

#ifndef __cplusplus
int
fn10 (int a, char x[static sizeof a])
{
  return sizeof x; /* { dg-warning "on array function parameter" "" { target c } } */
}

int
fn11 (a)
  char a[];
{
  return sizeof a; /* { dg-warning "on array function parameter" "" { target c } } */
}

int
fn12 (a)
  char *a;
{
  return sizeof a;
}
#endif

int
fn13 (char (*x)[2])
{
  return sizeof x;
}

int
fn14 (T t)
{
  return sizeof t; /* { dg-warning "on array function parameter" } */
}

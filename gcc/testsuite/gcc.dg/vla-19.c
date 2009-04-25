/* Test diagnostics for variably modified function return types.  PR
   39564.  */
/* { dg-do compile } */
/* { dg-options "-std=c99" } */

int a;

void
f1 (void)
{
  typedef int T[a];
  extern T *g1 (void); /* { dg-error "non-nested function with variably modified type" } */
}

void
f2 (void)
{
  extern int (*g2 (void))[a]; /* { dg-error "non-nested function with variably modified type" } */
}

void
f3 (void)
{
  typedef int T[a];
  T *g3 (void); /* { dg-error "non-nested function with variably modified type" } */
}

void
f4 (void)
{
  int (*g4 (void))[a]; /* { dg-error "non-nested function with variably modified type" } */
}

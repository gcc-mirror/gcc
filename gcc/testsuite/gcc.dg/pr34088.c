/* PR middle-end/34088 */
/* { dg-do compile } */
/* { dg-options "-O -Wall -Werror" } */

int F0 (int);
int F1 (int t) { return F0(t); }
int F2 (int t) { return F0(t); }

extern int X[];
static inline int foo(int i)
{
  return X[i];
}

int bar(int* p)
{
  int i;

  while ( ({ if (foo(*p) && foo(*p)); p; }) );

  return i;	/* { dg-error "is used uninitialized" } */
}

/* { dg-message "warnings being treated as errors" "" {target "*-*-*"} 0 } */

/* { dg-do compile } */
/* { dg-options "-O2 -Wmaybe-uninitialized" } */

/* PR tree-optimization/108466 */
/* The location of the uninitialized warning was missing due
   to phi factoring.  */

typedef unsigned long long T;

T f(void);
unsigned char g(void)
{
  T a;

  if (f())
    a = f();
  if (f())
    return 0;
  else
    return a; /* { dg-warning "may be used" } */
}



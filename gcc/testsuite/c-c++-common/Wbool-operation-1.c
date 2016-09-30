/* PR c/77490 */
/* { dg-do compile } */
/* { dg-options "-Wall -Wno-psabi" } */

#ifndef __cplusplus
# define bool _Bool
#endif

typedef volatile bool T;
typedef int __attribute__ ((vector_size (4 * sizeof (int)))) v4si;
extern bool foo (void);

int
fn (bool b, bool b2, T b3, int n, v4si v)
{
  int r = 0;

  r += ~b; /* { dg-warning "on an expression of type bool|on a boolean expression" } */
  r += n + ~b; /* { dg-warning "on an expression of type bool|on a boolean expression" } */
  r += ~(n == 1); /* { dg-warning "on an expression of type bool|on a boolean expression" } */
  r += ~(n || 1); /* { dg-warning "on an expression of type bool|on a boolean expression" } */
  r += ~b == 1; /* { dg-warning "on an expression of type bool|on a boolean expression" } */
  r += ~(++n, n == 1); /* { dg-warning "on an expression of type bool|on a boolean expression" } */
  r += ~(++n, n > 1); /* { dg-warning "on an expression of type bool|on a boolean expression" } */
  r += ~(++n, n && 1); /* { dg-warning "on an expression of type bool|on a boolean expression" } */
  r += (++n, ~b); /* { dg-warning "on an expression of type bool|on a boolean expression" } */
  r += ~b3; /* { dg-warning "on an expression of type bool|on a boolean expression" } */
  r += ~foo (); /* { dg-warning "on an expression of type bool|on a boolean expression" } */
  r += ~(bool) !1; /* { dg-warning "on an expression of type bool|on a boolean expression" } */

  v = ~v;
  r += ~(int) b;
  r += -b;

  return r;
}

/* { dg-prune-output ".*GCC vector passed by reference.*" } */

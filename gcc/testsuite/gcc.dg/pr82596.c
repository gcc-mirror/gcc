/* PR tree-optimization/82596 - missing -Warray-bounds on an out-of-bounds
   index into string literal
   { dg-do compile }
   { dg-options "-O2 -Warray-bounds" } */

#define SIZE_MAX  __SIZE_MAX__
#define SSIZE_MAX __PTRDIFF_MAX__
#define SSIZE_MIN (-SSIZE_MAX - 1)

void sink (int, ...);

#define T(arg) sink (arg)

void test_cststring (int i)
{
  T (""[SSIZE_MIN]);                      /* { dg-warning "below array bounds" "string" { xfail lp64 } } */
  T (""[SSIZE_MIN + 1]);                  /* { dg-warning "below array bounds" "string" } */
  T (""[-1]);                             /* { dg-warning "below array bounds" "string" } */
  T (""[0]);
  T (""[1]);                              /* { dg-warning "above array bounds" "string" } */
  T ("0"[2]);                             /* { dg-warning "above array bounds" "string" } */
  T ("012"[2]);
  T ("012"[3]);
  T ("012"[4]);                           /* { dg-warning "above array bounds" "string" } */
  T ("0123"[SSIZE_MAX]);                  /* { dg-warning "above array bounds" "string" } */
  T ("0123"[SIZE_MAX]);                   /* { dg-warning "above array bounds" "string" } */
}

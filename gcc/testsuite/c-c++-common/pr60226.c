/* PR c/60226 */
/* { dg-do compile } */
/* { dg-options "-Wno-c++-compat" { target c } } */
/* { dg-require-effective-target int32plus } */

typedef int __attribute__ ((aligned (1 << 28))) int28;
int28 foo[4] = {}; /* { dg-error "alignment of array elements is greater than element size" } */
typedef int __attribute__ ((aligned (1 << 29))) int29; /* { dg-error "requested alignment is too large|maximum object file alignment" } */

void
f (void)
{
  struct { __attribute__((aligned (1 << 28))) double a; } x1;
  struct { __attribute__((aligned (1 << 29))) double a; } x2; /* { dg-error "requested alignment is too large" } */
}

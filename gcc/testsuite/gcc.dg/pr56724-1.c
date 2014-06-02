/* PR c/56724 */
/* { dg-do compile } */
/* { dg-options "-Wtraditional-conversion" } */

extern void foo (int p[2][]); /* { dg-error "array type has incomplete element type" } */
extern void foo_i (int, int);
extern void foo_u (unsigned int);
extern void foo_f (int, float);
extern void foo_ll (long long);
extern void foo_cd (int, int, __complex__ double);
extern signed char sc;
extern int i;
extern unsigned int u;
extern float f;
extern double d;
extern __complex__ double cd;

void
fn ()
{
  int p[1][1];
  foo (p); /* { dg-error "8:type of formal parameter" } */
  foo_i (1, f); /* { dg-warning "13:passing argument" } */
  foo_i (1, cd); /* { dg-warning "13:passing argument" } */
  foo_cd (1, 2, f); /* { dg-warning "17:passing argument" } */
  foo_f (9, i); /* { dg-warning "13:passing argument" } */
  foo_cd (2, 2, i); /* { dg-warning "17:passing argument" } */
  foo_f (2, cd); /* { dg-warning "13:passing argument" } */
  foo_f (2, d); /* { dg-warning "13:passing argument" } */
  foo_ll (sc); /* { dg-warning "11:passing argument" } */
  foo_u (i); /* { dg-warning "10:passing argument" } */
  foo_i (1, u); /* { dg-warning "13:passing argument" } */
}

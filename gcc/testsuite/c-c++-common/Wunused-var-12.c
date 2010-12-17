/* PR c++/44780 */
/* { dg-do compile } */
/* { dg-options "-Wunused" } */
/* { dg-options "-Wunused -fno-common" { target { { hppa*-*-hpux* } && { ! hppa*64*-*-* } } } } */

typedef double vec __attribute__ ((__vector_size__ (16)));
vec c, d;

void
foo (void)
{
  vec a;
  vec b;
  a = c;
  b = a;
  d = b;
}

void
bar (void)
{
  vec a;
  vec b;	/* { dg-warning "set but not used" } */
  a = c;
  b = a;
}

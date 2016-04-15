/* PR c/70671 */
/* { dg-do compile } */

extern void bar (int *);

struct S
{
  int x:2;
} s, *r;

void
foo (void)
{
  int *p1 = &s.x; /* { dg-error "13:cannot take address of bit-field 'x'" } */
  int *p2;
  p2 = &s.x; /* { dg-error "8:cannot take address of bit-field 'x'" } */
  bar (&s.x); /* { dg-error "8:cannot take address of bit-field 'x'" } */
}

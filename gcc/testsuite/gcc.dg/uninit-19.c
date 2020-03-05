/* { dg-do compile } */
/* { dg-options "-O -Wuninitialized" } */
/* { dg-additional-options "-finline-small-functions" { target avr-*-* } } */

int a, l, m;
float *b;
float c, d, e, g, h;
unsigned char i, k;
void
fn1 (int p1, float *f1, float *f2, float *f3, unsigned char *c1, float *f4,
     unsigned char *c2, float *p10)
{
  if (p1 & 8)
    b[3] = p10[a];
  /* { dg-warning "may be used uninitialized" "" { target { { nonpic || pie_enabled } || { hppa*64*-*-* *-*-darwin* } } } .-1 } */
}

void
fn2 ()
{
  float *n;
  if (l & 6)
    n = &c + m;
  fn1 (l, &d, &e, &g, &i, &h, &k, n);
  /* { dg-warning "may be used uninitialized" "" { target { ! { { nonpic || pie_enabled } || { hppa*64*-*-* *-*-darwin* } } } } .-1 } */
}

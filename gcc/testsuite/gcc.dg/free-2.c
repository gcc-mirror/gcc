/* PR c/36970 */
/* { dg-do compile } */
/* { dg-options "-O0" } */

extern void free (void *);

char *p, buf3[10], d;
struct S { char a; int b; } *r;

void foo (void)
{
  char buf[10], buf2[10], c;
  static char buf4[10], e;
  char *q = buf;
  free (p);
  free (q);	      /* At -O0 no warning is reported here.  */
  free (buf2);	      /* { dg-warning "attempt to free a non-heap object" } */
  free (&c);	      /* { dg-warning "attempt to free a non-heap object" } */
  free (buf3);	      /* { dg-warning "attempt to free a non-heap object" } */
  free (&d);	      /* { dg-warning "attempt to free a non-heap object" } */
  free (buf4);	      /* { dg-warning "attempt to free a non-heap object" } */
  free (&e);	      /* { dg-warning "attempt to free a non-heap object" } */
  free (&r->a);
  free ("abcd");      /* { dg-warning "attempt to free a non-heap object" } */
  free (L"abcd");     /* { dg-warning "attempt to free a non-heap object" } */
}

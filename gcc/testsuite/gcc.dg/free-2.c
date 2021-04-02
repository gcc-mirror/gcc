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
  free (q);	      /* { dg-warning "\\\[-Wfree-nonheap-object" } */
  free (buf2);	      /* { dg-warning "\\\[-Wfree-nonheap-object" } */
  free (&c);	      /* { dg-warning "\\\[-Wfree-nonheap-object" } */
  free (buf3);	      /* { dg-warning "\\\[-Wfree-nonheap-object" } */
  free (&d);	      /* { dg-warning "\\\[-Wfree-nonheap-object" } */
  free (buf4);	      /* { dg-warning "\\\[-Wfree-nonheap-object" } */
  free (&e);	      /* { dg-warning "\\\[-Wfree-nonheap-object" } */
  free (&r->a);
  free ("abcd");      /* { dg-warning "\\\[-Wfree-nonheap-object" } */
  free (L"abcd");     /* { dg-warning "\\\[-Wfree-nonheap-object" } */
}

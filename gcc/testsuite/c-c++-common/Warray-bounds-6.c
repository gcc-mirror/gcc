/* PR tree-optimization/86614 */
/* { dg-do compile } */
/* { dg-options "-O2 -Warray-bounds" } */

extern char *strncpy (char *, const char *, __SIZE_TYPE__);

void sink (void *);

struct A { char b[17]; } a[2];

void g (const char *s, unsigned n)
{
  int i = (char *)a[1].b - (char *)a + 1;
  char *d = a[1].b;
  /* Ensure the same bug is not diagnosed more than once.  */
  strncpy (d + i, s, n);	/* { dg-warning "array subscript \[0-9]+ is outside array bounds of" } */
				/* { dg-bogus "offset \[0-9]+ is out of the bounds \\\[0, \[0-9]+\\\] of object 'a' with type" "" { target *-*-* } .-1 } */
}

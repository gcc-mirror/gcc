/* PR tree-optimization/86614 - duplicate -Warray-bounds for a strncpy
   call with out-of-bounds offset
   { dg-do compile }
   { dg-options "-O2 -Warray-bounds" } */

#if __cplusplus
extern "C"
#endif
char *strncpy (char *, const char *, __SIZE_TYPE__);

void sink (void *);

struct A { char b[17]; } a[2];

void g (const char *s, unsigned n)
{
  int i = (char *)a[1].b - (char *)a + 1;
  char *d = a[1].b;
  /* Verify the bug is diagnosed exactly once, using either form
     of the warning.  */
  strncpy (d + i, s, n);	/* { dg-warning "array subscript \[0-9]+ is outside array bounds|offset \[0-9]+ is out of the bounds" } */
				/* { dg-bogus "offset \[0-9]+ is out of the bounds|array subscript \[0-9]+ is outside array bounds" "" { target *-*-* } .-1 } */
}

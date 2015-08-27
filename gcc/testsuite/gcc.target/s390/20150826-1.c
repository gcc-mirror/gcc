/* Check that emitting a dynamic stack check for sizes below the
   current frame size work.  */

/* { dg-do compile } */
/* { dg-options "-O3 -mstack-size=32" } */

extern int bar(char *);
int foo(void)
{
  char b[100];
  return bar(b);
} /* { dg-warning "An unconditional trap is added" } */

/* Test for format attributes: test default attributes are silently ignored
   when a function is redeclared in an inappropriate manner.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=gnu99 -Wformat" } */

/* We can't #include "format.h" here.  */

/* This scanf declaration is static, so can't be the system function.  */
static int scanf(const char *restrict, ...);

/* This sscanf declaration doesn't have variable arguments, so isn't
   compatible with a format attribute.  */
extern int sscanf(const char *restrict, const char *restrict, int *); /* { dg-warning "conflict" "conflict" } */

void
foo (const char *s, int *p)
{
  scanf("%ld", p); /* { dg-bogus "format" "static" } */
  sscanf(s, "%ld", p); /* { dg-bogus "format" "wrong type" } */
}

/* Dummy definition of scanf.  */
static int
scanf (const char *restrict fmt, ...)
{
  return 0;
}

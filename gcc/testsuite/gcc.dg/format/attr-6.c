/* Test for format attributes: test default attributes are applied
   to implicit declarations.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=gnu89 -Wformat" } */

/* We can't #include "format.h" here.  */

/* Technically, none of the format functions should be implicitly declared;
   either the implicit type is wrong, the function has variable arguments
   or it requires a type declared in a header.  However, some bad programming
   practice uses implicit declarations of some of these functions.

   Note that printf is not used in this test because of the declaration
   of it as a built-in function.  */

void
foo (const char *s, int *p)
{
  scanf("%ld", p); /* { dg-warning "12:format" "implicit scanf" } */
  /* { dg-warning "implicit" "implicit decl warning" { target *-*-* } .-1 } */
}

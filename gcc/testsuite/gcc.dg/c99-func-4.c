/* Test for C99 __func__: of type const char [].  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

void
foo (void)
{
  char *p = __func__; /* { dg-error "discards" "__func__ pointer to const" } */
}

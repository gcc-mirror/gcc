/* Test for warnings for non-string-literal formats.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=gnu99 -Wformat -Wformat-nonliteral" } */

extern int printf (const char *, ...);

void
foo (char *s, __SIZE_TYPE__ i)
{
  printf ((const char *)i, i); /* { dg-warning "argument types" "non-literal" } */
  printf (s, i); /* { dg-warning "argument types" "non-literal" } */
}

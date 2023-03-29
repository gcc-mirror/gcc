/* { dg-additional-options "-Wno-stringop-overflow -Wno-stringop-truncation" } */
#include <string.h>

/* Wanalyzer-out-of-bounds tests for strpy-related overflows.
  
   The intra-procedural tests are all caught by Wstringop-overflow.
   The inter-procedural out-of-bounds are only found by the analyzer.  */

void test1 (void)
{
  char dst[5];
  strcpy (dst, "Hello"); /* { dg-line test1 } */

  /* { dg-warning "stack-based buffer overflow" "warning" { target *-*-* } test1 } */
  /* { dg-message "write of 1 byte to beyond the end of 'dst'" "num bad bytes note" { target *-*-* } test1 } */
  /* { dg-message "valid subscripts for 'dst' are '\\\[0\\\]' to '\\\[4\\\]'" "valid subscript note" { target *-*-* } test1 } */
}

void test2 (void)
{
  char dst[6];
  strcpy (dst, "Hello");
}

void test3 (void)
{
  char *src = "Hello";
  char dst[5];
  strcpy (dst, src); /* { dg-line test3 } */

  /* { dg-warning "stack-based buffer overflow" "warning" { target *-*-* } test3 } */
  /* { dg-message "write of 1 byte to beyond the end of 'dst'" "num bad bytes note" { target *-*-* } test3 } */
  /* { dg-message "valid subscripts for 'dst' are '\\\[0\\\]' to '\\\[4\\\]'" "valid subscript note" { target *-*-* } test3 } */
}

void test4 (void)
{
  char *src = "Hello";
  char dst[6];
  strcpy (dst, src);
}

const char *return_hello (void)
{
  return "hello";
}

void test5 (void)
{
  const char *str = return_hello ();
  if (!str)
    return;
  char dst[5];
  strcpy (dst, str); /* { dg-line test5 } */

  /* { dg-warning "stack-based buffer overflow" "warning" { target *-*-* } test5 } */
  /* { dg-message "write of 1 byte to beyond the end of 'dst'" "num bad bytes note" { target *-*-* } test5 } */
  /* { dg-message "valid subscripts for 'dst' are '\\\[0\\\]' to '\\\[4\\\]'" "valid subscript note" { target *-*-* } test5 } */
}

void test6 (void)
{
  const char *str = return_hello ();
  if (!str)
    return;
  char dst[6];
  strcpy (dst, str);
}

/* Test for format checking of constant arrays.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=gnu99 -Wformat=2" } */

#include "format.h"

const char a1[] = "foo";
const char a2[] = "foo%d";
const char b[3] = "foo";
static const char c1[] = "foo";
static const char c2[] = "foo%d";
char d[] = "foo";
volatile const char e[] = "foo";

void
foo (int i, long l)
{
  const char p1[] = "bar";
  const char p2[] = "bar%d";
  static const char q1[] = "bar";
  static const char q2[] = "bar%d";
  printf (a1);
  printf (a2, i);
  printf (a2, l); /* { dg-warning "format" "wrong type with array" } */
  printf (b); /* { dg-warning "unterminated" "unterminated array" } */
  printf (c1);
  printf (c2, i);
  printf (c2, l); /* { dg-warning "format" "wrong type with array" } */
  printf (p1);
  printf (p2, i);
  printf (p2, l); /* { dg-warning "format" "wrong type with array" } */
  printf (q1);
  printf (q2, i);
  printf (q2, l); /* { dg-warning "format" "wrong type with array" } */
  /* Volatile or non-constant arrays must not be checked.  */
  printf (d); /* { dg-warning "not a string literal" "non-const" } */
  printf ((const char *)e); /* { dg-warning "not a string literal" "volatile" } */
}

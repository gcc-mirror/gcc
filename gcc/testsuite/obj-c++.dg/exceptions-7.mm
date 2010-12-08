/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, December 2010.  */
/* { dg-options "-fobjc-exceptions" } */
/* { dg-do compile } */

/* Test warnings when the argument of @throw is invalid.  */

#include <objc/objc.h>

void test (id object)
{
  struct x { int i; } invalid_1, *invalid_2;

  @throw object;    /* Ok */
  @throw 1;         /* { dg-error ".@throw. argument is not an object" } */
  @throw "string";  /* { dg-error ".@throw. argument is not an object" } */
  @throw invalid_1; /* { dg-error ".@throw. argument is not an object" } */
  @throw invalid_2; /* { dg-error ".@throw. argument is not an object" } */
}

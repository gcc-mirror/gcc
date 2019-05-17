/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, November 2010.  */
/* { dg-options "-fobjc-exceptions" } */
/* { dg-do compile } */

/* Test warnings when parsing syntax errors in @throw.  */

#include <objc/objc.h>

void test (id object)
{
  @throw object;   /* Ok */
  @throw;          /* { dg-error ".@throw. .rethrow. used outside of a '@catch' block" } */
  @throw (object); /* Ok.  */
  @throw (id)0     /* { dg-error "expected" } */
}

void test2 (id object)
{
  @throw object);  /* { dg-error "expected" } */
  @throw (...);    /* { dg-error "expected" } */
  @throw ();       /* { dg-error "expected" } */
  @throw           /* { dg-error "expected" } */
}                  /* { dg-error "expected" } */

void test3 (id object1, id object2)
{
  /* This is apparently valid.  */
  @throw object1, object2; /* Ok.  */
}

/* Test if caught exception objects are accessible inside the
   @catch block.  (Yes, I managed to break this.)  */
/* Author: Ziemowit Laski <zlaski@apple.com> */

/* { dg-do compile } */
/* { dg-options "-fobjc-exceptions" } */

#include "../objc-obj-c++-shared/TestsuiteObject.h"

const char *foo(void)
{
  @try {
    return "foo";
  }
  @catch (TestsuiteObject* theException) {
    return [theException name];
  }
  return (const char *)0;
}

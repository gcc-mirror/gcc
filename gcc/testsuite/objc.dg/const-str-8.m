/* Test for assigning compile-time constant-string objects to static variables.  */
/* Contributed by Ziemowit Laski <zlaski@apple.com>  */
/* { dg-options "-fconstant-string-class=Foo" } */
/* { dg-do run } */

#include "../objc-obj-c++-shared/Object1.h"
#include <stdlib.h>

@interface Foo: Object {
  char *cString;
  unsigned int len;
}
@end

#ifndef NEXT_OBJC_USE_NEW_INTERFACE
struct objc_class _FooClassReference;
#else
Class _FooClassReference;
#endif

@implementation Foo : Object
- (char *)customString {
  return cString;
}
@end

static const Foo *appKey = @"MyApp";
static int CFPreferencesSynchronize (const Foo *ref) {
  return ref == appKey;
}

static void PrefsSynchronize(void)
{
  if(!CFPreferencesSynchronize(appKey))
    abort();
}

int main () {
  PrefsSynchronize();
  return 0;
}

#include "../objc-obj-c++-shared/Object1-implementation.h"

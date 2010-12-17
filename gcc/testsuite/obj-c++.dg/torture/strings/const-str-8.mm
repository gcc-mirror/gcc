/* Test for assigning compile-time constant-string objects to static variables.  */
/* Contributed by Ziemowit Laski <zlaski@apple.com>  */

/* { dg-do run { target *-*-darwin* } } */
/* { dg-options "-fconstant-string-class=Foo" } */
/* { dg-options "-mno-constant-cfstrings -fconstant-string-class=Foo" { target *-*-darwin* } } */
/* { dg-additional-sources "../../../objc-obj-c++-shared/Object1.mm" } */

#include "../../../objc-obj-c++-shared/Object1.h"
#include <stdlib.h>

@interface Foo: Object {
  char *cString;
  unsigned int len;
}
@end

#ifdef NEXT_OBJC_USE_NEW_INTERFACE
Class _FooClassReference;
#else
struct objc_class _FooClassReference;
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

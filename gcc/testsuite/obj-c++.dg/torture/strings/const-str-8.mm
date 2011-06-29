/* Test for assigning compile-time constant-string objects to static variables.  */
/* Contributed by Ziemowit Laski <zlaski@apple.com>  */

/* { dg-do run { target *-*-darwin* } } */
/* { dg-options "-fconstant-string-class=Foo" } */
/* { dg-options "-mno-constant-cfstrings -fconstant-string-class=Foo" { target *-*-darwin* } } */

#include "../../../objc-obj-c++-shared/TestsuiteObject.m"
#include <stdlib.h>

@interface Foo: TestsuiteObject {
  char *cString;
  unsigned int len;
}
@end

#ifdef __NEXT_RUNTIME__
#ifdef NEXT_OBJC_USE_NEW_INTERFACE
Class _FooClassReference;
#else
struct objc_class _FooClassReference;
#endif
#endif

@implementation Foo : TestsuiteObject
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

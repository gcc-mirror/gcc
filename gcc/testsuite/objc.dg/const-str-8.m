/* Test for assigning compile-time constant-string objects to static variables.  */
/* Contributed by Ziemowit Laski <zlaski@apple.com>  */

/* { dg-options "-fnext-runtime -fconstant-string-class=Foo -lobjc" } */
/* { dg-do run { target *-*-darwin* } } */


#include <stdlib.h>
#include <objc/Object.h>

@interface Foo: Object {
  char *cString;
  unsigned int len;
}
@end

struct objc_class _FooClassReference;

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

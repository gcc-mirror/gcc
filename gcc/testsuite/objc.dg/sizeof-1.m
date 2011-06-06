/* Check that the sizeof() operator works with ObjC classes and their aliases. */
/* Contributed by Ziemowit Laski <zlaski@apple.com>.  */
/* { dg-options "" } */
/* { dg-do run } */

#include "../objc-obj-c++-shared/TestsuiteObject.m"
#include <objc/objc.h>

extern void abort(void);
#define CHECK_IF(expr) if(!(expr)) abort();

@interface Foo: TestsuiteObject {
  int a, b;
  float c, d;
}
@end

@implementation Foo
@end

typedef TestsuiteObject MyObject;
typedef struct Foo Foo_type;

@compatibility_alias AliasObject TestsuiteObject;

int main(void) {
  CHECK_IF(sizeof(Foo) > sizeof(TestsuiteObject) && sizeof(TestsuiteObject) > 0);
  CHECK_IF(sizeof(Foo) == sizeof(Foo_type));
  CHECK_IF(sizeof(TestsuiteObject) == sizeof(MyObject));
  CHECK_IF(sizeof(TestsuiteObject) == sizeof(AliasObject));
  return 0;
}


/* Check that the sizeof() operator works with ObjC classes and their aliases. */
/* Contributed by Ziemowit Laski <zlaski@apple.com>.  */
/* { dg-options "" } */
/* { dg-do run } */

#include "../objc-obj-c++-shared/Object1.h"
#include <objc/objc.h>

extern void abort(void);
#define CHECK_IF(expr) if(!(expr)) abort();

@interface Foo: Object {
  int a, b;
  float c, d;
}
@end

@implementation Foo
@end

typedef Object MyObject;
typedef struct Foo Foo_type;

@compatibility_alias AliasObject Object;

int main(void) {
  CHECK_IF(sizeof(Foo) > sizeof(Object) && sizeof(Object) > 0);
  CHECK_IF(sizeof(Foo) == sizeof(Foo_type));
  CHECK_IF(sizeof(Object) == sizeof(MyObject));
  CHECK_IF(sizeof(Object) == sizeof(AliasObject));
  return 0;
}

#include "../objc-obj-c++-shared/Object1-implementation.h"
